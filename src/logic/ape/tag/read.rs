use super::{ApeItem, ApeTag};
use crate::error::{LoftyError, Result};
use crate::logic::ape::constants::INVALID_KEYS;
use crate::types::item::ItemValue;

use std::io::{Read, Seek, SeekFrom};
use std::ops::Neg;

use byteorder::{LittleEndian, ReadBytesExt};

pub(crate) fn read_ape_tag<R>(data: &mut R, footer: bool) -> Result<(ApeTag, u32)>
where
	R: Read + Seek,
{
	let version = data.read_u32::<LittleEndian>()?;

	let mut size = data.read_u32::<LittleEndian>()?;

	if size < 32 {
		// If the size is < 32, something went wrong during encoding
		// The size includes the footer and all items
		return Err(LoftyError::Ape("Tag has an invalid size (< 32)"));
	}

	let item_count = data.read_u32::<LittleEndian>()?;

	if footer {
		// No point in reading the rest of the footer, just seek back to the end of the header
		data.seek(SeekFrom::Current(i64::from(size - 12).neg()))?;
	} else {
		// There are 12 bytes remaining in the header
		// Flags (4)
		// Reserved (8)
		data.seek(SeekFrom::Current(12))?;
	}

	let mut tag = ApeTag::default();

	for _ in 0..item_count {
		let value_size = data.read_u32::<LittleEndian>()?;

		if value_size == 0 {
			return Err(LoftyError::Ape("Tag item value has an invalid size (0)"));
		}

		let flags = data.read_u32::<LittleEndian>()?;

		let mut key = Vec::new();
		let mut key_char = data.read_u8()?;

		while key_char != 0 {
			key.push(key_char);
			key_char = data.read_u8()?;
		}

		let key = String::from_utf8(key)
			.map_err(|_| LoftyError::Ape("Tag item contains a non UTF-8 key"))?;

		if INVALID_KEYS.contains(&&*key.to_uppercase()) {
			return Err(LoftyError::Ape("Tag item contains an illegal key"));
		}

		if key.chars().any(|c| !c.is_ascii()) {
			return Err(LoftyError::Ape("Tag item contains a non ASCII key"));
		}

		let read_only = (flags & 1) == 1;

		let item_type = (flags & 6) >> 1;

		let mut value = vec![0; value_size as usize];
		data.read_exact(&mut value)?;

		let parsed_value = match item_type {
			0 => ItemValue::Text(String::from_utf8(value).map_err(|_| {
				LoftyError::Ape("Expected a string value based on flags, found binary data")
			})?),
			1 => ItemValue::Binary(value),
			2 => ItemValue::Locator(String::from_utf8(value).map_err(|_| {
				LoftyError::Ape("Failed to convert locator item into a UTF-8 string")
			})?),
			_ => return Err(LoftyError::Ape("Tag item contains an invalid item type")),
		};

		let mut item = ApeItem::new(key, parsed_value)?;

		if read_only {
			item.set_read_only()
		}

		tag.insert(item);
	}

	// Version 1 doesn't include a header
	if version == 2000 {
		size += 32
	}

	// Skip over footer
	data.seek(SeekFrom::Current(32))?;

	Ok((tag, size))
}
