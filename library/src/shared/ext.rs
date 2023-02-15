//! Extension traits.

use crate::prelude::*;

/// Additional methods on content.
pub trait ContentExt {
    /// Make this content strong.
    fn strong(self) -> Self;

    /// Make this content emphasized.
    fn emph(self) -> Self;

    /// Underline this content.
    fn underlined(self) -> Self;

    /// Link the content to a destination.
    fn linked(self, dest: Destination) -> Self;

    /// Set alignments for this content.
    fn aligned(self, aligns: Axes<Option<GenAlign>>) -> Self;

    /// Pad this content at the sides.
    fn padded(self, padding: Sides<Rel<Length>>) -> Self;

    /// Transform this content's contents without affecting layout.
    fn moved(self, delta: Axes<Rel<Length>>) -> Self;
}

impl ContentExt for Content {
    fn strong(self) -> Self {
        crate::text::StrongNode(self).pack()
    }

    fn emph(self) -> Self {
        crate::text::EmphNode(self).pack()
    }

    fn underlined(self) -> Self {
        crate::text::UnderlineNode(self).pack()
    }

    fn linked(self, dest: Destination) -> Self {
        self.styled(Meta::DATA, vec![Meta::Link(dest.clone())])
    }

    fn aligned(self, aligns: Axes<Option<GenAlign>>) -> Self {
        self.styled(crate::layout::AlignNode::ALIGNS, aligns)
    }

    fn padded(self, padding: Sides<Rel<Length>>) -> Self {
        crate::layout::PadNode { padding, body: self }.pack()
    }

    fn moved(self, delta: Axes<Rel<Length>>) -> Self {
        crate::layout::MoveNode { delta, body: self }.pack()
    }
}

/// Additional methods for style maps.
pub trait StyleMapExt {
    /// Set a font family composed of a preferred family and existing families
    /// from a style chain.
    fn set_family(&mut self, preferred: crate::text::FontFamily, existing: StyleChain);
}

impl StyleMapExt for StyleMap {
    fn set_family(&mut self, preferred: crate::text::FontFamily, existing: StyleChain) {
        self.set(
            crate::text::TextNode::FAMILY,
            crate::text::FallbackList(
                std::iter::once(preferred)
                    .chain(existing.get(crate::text::TextNode::FAMILY).0.iter().cloned())
                    .collect(),
            ),
        );
    }
}
