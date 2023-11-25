use crate::ebml::vint::VInt;
use crate::error::Result;
use crate::macros::decode_err;

use std::io::Read;

use byteorder::{BigEndian, ReadBytesExt};
use lofty_attr::ebml_master_elements;

pub struct ElementHeader {
	pub(crate) id: VInt,
	pub(crate) size: VInt,
}

impl ElementHeader {
	fn read<R>(reader: &mut R, max_id_length: u8, max_vint_length: u8) -> Result<Self>
	where
		R: Read,
	{
		Ok(Self {
			id: VInt::parse_from_element_id(reader, max_id_length)?,
			size: VInt::parse(reader, max_vint_length)?,
		})
	}
}

#[derive(Copy, Clone)]
pub enum ElementDataType {
	SignedInt,
	UnsignedInt,
	Float,
	String,
	Utf8,
	Date,
	Master,
	Binary,
}

#[derive(Copy, Clone)]
struct MasterElement {
	id: ElementIdent,
	children: &'static [(VInt, ChildElementDescriptor)],
}

#[derive(Copy, Clone)]
pub(crate) struct ChildElementDescriptor {
	pub(crate) ident: ElementIdent,
	pub(crate) data_type: ElementDataType,
}

ebml_master_elements! {
	EBML: {
		id: 0x1A45_DFA3,
		children: [
			EBMLVersion: { 0x4286, UnsignedInt },
			EBMLReadVersion: { 0x42F7, UnsignedInt },
			EBMLMaxIDLength: { 0x42F2, UnsignedInt },
			EBMLMaxSizeLength: { 0x42F3, UnsignedInt },
			DocType: { 0x4282, String },
			DocTypeVersion: { 0x4287, UnsignedInt },
			DocTypeReadVersion: { 0x4285, UnsignedInt },
		],
	},
	DocTypeExtension: {
		id: 0x4281,
		children: [
			DocTypeExtensionName: { 0x4283, String },
			DocTypeExtensionVersion: { 0x4284, UnsignedInt },
		],
	},

	// The Root Element that contains all other Top-Level Elements
	Segment: {
		id: 0x1853_8067,
		children: [
			Info: { 0x1549_A966, Master },
			Cluster: { 0x1F43_B675, Master },
			Tracks: { 0x1654_AE6B, Master },
			Tags: { 0x1254_C367, Master },
			Attachments: { 0x1941_A469, Master },
			Chapters: { 0x1043_A770, Master },
		],
	},

	// segment.info
	Info: {
		id: 0x1549_A966,
		children: [
			TimecodeScale: { 0x2AD7_B1, UnsignedInt },
			MuxingApp: { 0x4D80, Utf8 },
			WritingApp: { 0x5741, Utf8 },
		],
	},

	// segment.tracks
	Tracks: {
		id: 0x1654_AE6B,
		children: [
			TrackEntry: { 0xAE, Master },
		],
	},

	// segment.tracks.trackEntry
	TrackEntry: {
		id: 0xAE,
		children: [
			TrackType: { 0x83, UnsignedInt },
			FlagEnabled: { 0xB9, UnsignedInt },
			FlagDefault: { 0x88, UnsignedInt },
			FlagLacing: { 0x9C, UnsignedInt },
			DefaultDuration: { 0x23E3_83, UnsignedInt },
			TrackTimecodeScale: { 0x2331_59, Float },
			MaxBlockAdditionID: { 0x55EE, UnsignedInt },
			Language: { 0x22B5_9C, String },
			CodecID: { 0x86, String },
			CodecDelay: { 0x56AA, UnsignedInt },
			SeekPreRoll: { 0x56BB, UnsignedInt },
			TrackTranslate: { 0x6624, Master },
			Video: { 0xE0, Master },
			Audio: { 0xE1, Master },
			TrackOperation: { 0xE2, Master },
			ContentEncodings: { 0x6D80, Master },
		],
	},

	// segment.attachments
	Attachments: {
		id: 0x1941_A469,
		children: [
			AttachedFile: { 0x61A7, Master },
		],
	},

	// segment.attachments.attachedFile
	AttachedFile: {
		id: 0x61A7,
		children: [
			FileDescription: { 0x467E, String },
			FileName: { 0x466E, Utf8 },
			FileMimeType: { 0x4660, String },
			FileData: { 0x465C, Binary },
			FileUID: { 0x46AE, UnsignedInt },
			FileReferral: { 0x4675, Binary },
			FileUsedStartTime: { 0x4661, UnsignedInt },
			FileUsedEndTime: { 0x4662, UnsignedInt },
		],
	},
}

struct ElementReaderContext {
	/// Current master element
	current_master: Option<MasterElement>,
	/// Remaining length of the master element
	master_length: u64,
	/// Maximum size in octets of all element IDs
	max_id_length: u8,
	/// Maximum size in octets of all element data sizes
	max_size_length: u8,
}

impl Default for ElementReaderContext {
	fn default() -> Self {
		Self {
			current_master: None,
			master_length: 0,
			// https://www.rfc-editor.org/rfc/rfc8794.html#name-ebmlmaxidlength-element
			max_id_length: 4,
			// https://www.rfc-editor.org/rfc/rfc8794.html#name-ebmlmaxsizelength-element
			max_size_length: 8,
		}
	}
}

pub(crate) enum ElementReaderYield {
	Master((ElementIdent, u64)),
	Child((ChildElementDescriptor, u64)),
	Unknown(ElementHeader),
	Eof,
}

pub struct ElementReader<R> {
	reader: R,
	ctx: ElementReaderContext,
}

impl<R> ElementReader<R>
where
	R: Read,
{
	pub(crate) fn new(reader: R) -> Self {
		Self {
			reader,
			ctx: ElementReaderContext::default(),
		}
	}

	pub(crate) fn set_max_id_length(&mut self, len: u8) {
		self.ctx.max_id_length = len
	}

	pub(crate) fn set_max_size_length(&mut self, len: u8) {
		self.ctx.max_size_length = len
	}

	fn next_master(&mut self) -> Result<ElementReaderYield> {
		let header = ElementHeader::read(
			&mut self.reader,
			self.ctx.max_id_length,
			self.ctx.max_size_length,
		)?;
		let Some(master) = MASTER_ELEMENTS.get(&header.id) else {
			// We encountered an unknown master element
			return Ok(ElementReaderYield::Unknown(header));
		};

		self.ctx.current_master = Some(*master);
		self.ctx.master_length = header.size.value();
		Ok(ElementReaderYield::Master((
			master.id,
			self.ctx.master_length,
		)))
	}

	pub(crate) fn next(&mut self) -> Result<ElementReaderYield> {
		let Some(current_master) = self.ctx.current_master else {
			return self.next_master();
		};

		if self.ctx.master_length == 0 {
			return self.next_master();
		}

		let header = ElementHeader::read(
			&mut self.reader,
			self.ctx.max_id_length,
			self.ctx.max_size_length,
		)?;

		let Some((_, child)) = current_master
			.children
			.iter()
			.find(|(id, _)| *id == header.id)
		else {
			return Ok(ElementReaderYield::Unknown(header));
		};

		Ok(ElementReaderYield::Child((*child, header.size.value())))
	}

	pub(crate) fn skip(&mut self, length: u64) -> Result<()> {
		std::io::copy(&mut self.reader.by_ref().take(length), &mut std::io::sink())?;
		Ok(())
	}

	pub(crate) fn read_signed_int(&mut self) -> Result<i64> {
		todo!()
	}

	pub(crate) fn read_unsigned_int(&mut self) -> Result<u64> {
		todo!()
	}

	pub(crate) fn read_float(&mut self, element_length: u64) -> Result<f64> {
		Ok(match element_length {
			0 => 0.0,
			4 => f64::from(self.reader.read_f32::<BigEndian>()?),
			8 => self.reader.read_f64::<BigEndian>()?,
			_ => decode_err!(@BAIL Ebml, "Invalid size for float element"),
		})
	}

	pub(crate) fn read_string(&mut self) -> Result<String> {
		todo!()
	}

	pub(crate) fn read_utf8(&mut self) -> Result<String> {
		todo!()
	}

	pub(crate) fn read_date(&mut self) -> Result<String> {
		todo!()
	}

	pub(crate) fn read_binary(&mut self) -> Result<Vec<u8>> {
		todo!()
	}
}
