use crate::LinkedDataResource;

pub struct Ref<T>(pub T);

impl<T> From<T> for Ref<T> {
	fn from(value: T) -> Self {
		Self(value)
	}
}

impl<T: LinkedDataResource> LinkedDataResource for Ref<T> {
	fn interpretation(
		&self,
		vocabulary: &mut (),
		interpretation: &mut (),
	) -> crate::ResourceInterpretation<(), ()> {
		T::reference_interpretation(&self.0, vocabulary, interpretation)
	}
}