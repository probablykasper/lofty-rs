use std::cell::UnsafeCell;

thread_local! {
	static GLOBAL_OPTIONS: UnsafeCell<GlobalOptions> = UnsafeCell::new(GlobalOptions::default());
}

pub(crate) unsafe fn global_options() -> &'static GlobalOptions {
	GLOBAL_OPTIONS.with(|global_options| unsafe { &*global_options.get() })
}

/// Options that control all interactions with Lofty for the current thread
///
/// # Examples
///
/// ```rust
/// use lofty::config::{apply_global_options, GlobalOptions};
///
/// // I have a custom resolver that I need checked
/// let global_options = GlobalOptions::new().use_custom_resolvers(true);
/// apply_global_options(global_options);
/// ```
#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
#[non_exhaustive]
pub struct GlobalOptions {
	pub(crate) use_custom_resolvers: bool,
	pub(crate) allocation_limit: usize,
}

impl GlobalOptions {
	/// Default allocation limit for any single tag item
	pub const DEFAULT_ALLOCATION_LIMIT: usize = 16 * 1024 * 1024;

	/// Creates a new `GlobalOptions`, alias for `Default` implementation
	///
	/// See also: [`GlobalOptions::default`]
	///
	/// # Examples
	///
	/// ```rust
	/// use lofty::config::GlobalOptions;
	///
	/// let global_options = GlobalOptions::new();
	/// ```
	#[must_use]
	pub const fn new() -> Self {
		Self {
			use_custom_resolvers: true,
			allocation_limit: Self::DEFAULT_ALLOCATION_LIMIT,
		}
	}

	/// Whether or not to check registered custom resolvers
	///
	/// See also: [`crate::resolve`]
	///
	/// # Examples
	///
	/// ```rust
	/// use lofty::config::{apply_global_options, GlobalOptions};
	///
	/// // By default, `use_custom_resolvers` is enabled. Here, we don't want to use them.
	/// let global_options = GlobalOptions::new().use_custom_resolvers(false);
	/// apply_global_options(global_options);
	/// ```
	pub fn use_custom_resolvers(&mut self, use_custom_resolvers: bool) -> Self {
		self.use_custom_resolvers = use_custom_resolvers;
		*self
	}

	/// The maximum number of bytes to allocate for any single tag item
	///
	/// This is a safety measure to prevent allocating too much memory for a single tag item. If a tag item
	/// exceeds this limit, the allocator will return [`crate::error::ErrorKind::TooMuchData`].
	///
	/// # Examples
	///
	/// ```rust
	/// use lofty::config::{apply_global_options, GlobalOptions};
	///
	/// // I have files with gigantic images, I'll double the allocation limit!
	/// let global_options = GlobalOptions::new().allocation_limit(32 * 1024 * 1024);
	/// apply_global_options(global_options);
	/// ```
	pub fn allocation_limit(&mut self, allocation_limit: usize) -> Self {
		self.allocation_limit = allocation_limit;
		*self
	}
}

impl Default for GlobalOptions {
	/// The default implementation for `GlobalOptions`
	///
	/// The defaults are as follows:
	///
	/// ```rust,ignore
	/// GlobalOptions {
	/// 	use_custom_resolvers: true,
	/// 	allocation_limit: Self::DEFAULT_ALLOCATION_LIMIT,
	/// }
	/// ```
	fn default() -> Self {
		Self::new()
	}
}

/// Applies the given `GlobalOptions` to the current thread
///
/// # Examples
///
/// ```rust
/// use lofty::config::{apply_global_options, GlobalOptions};
///
/// // I have a custom resolver that I need checked
/// let global_options = GlobalOptions::new().use_custom_resolvers(true);
/// apply_global_options(global_options);
/// ```
pub fn apply_global_options(options: GlobalOptions) {
	GLOBAL_OPTIONS.with(|global_options| unsafe {
		*global_options.get() = options;
	});
}
