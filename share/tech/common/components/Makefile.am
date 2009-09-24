## Process this file with automake to produce Makefile.in

SUBDIRS = ctrl-broad ctrl-early data-single-broad data-single-early data-dual data-1of4

techcommoncompdir = $(datadir)/tech/common/components

techcommoncomp_DATA = \
	ActiveEagerFalseVariable.abs ActiveEagerNullAdapt.abs \
	Adapt.abs Arbiter.abs \
	Bar.abs BinaryFunc.abs BinaryFuncConstR.abs \
	BinaryFuncConstRPush.abs BinaryFuncPush.abs \
	Call.abs CallActive.abs CallDemux.abs CallDemuxPush.abs \
	CallMux.abs Case.abs CaseFetch.abs Combine.abs CombineEqual.abs \
	Concur.abs Constant.abs Continue.abs ContinuePush.abs \
	DecisionWait.abs Encode.abs FalseVariable.abs Fetch.abs \
	Fork.abs ForkPush.abs Halt.abs HaltPush.abs InitVariable.abs Loop.abs \
	Mask.abs NullAdapt.abs Passivator.abs PassivatorPush.abs \
	PassiveEagerFalseVariable.abs PassiveEagerNullAdapt.abs \
	PassiveSyncEagerFalseVariable.abs \
	Sequence.abs Split.abs SplitEqual.abs Synch.abs SynchPull.abs \
	SynchPush.abs UnaryFunc.abs UnaryFuncPush.abs Variable.abs While.abs \
	definitions.abs dual-rail-definitions.abs one-of-four-definitions.abs \
	WireFork.abs Slice.abs \
	BuiltinVariable.abs SequenceOptimised.abs

EXTRA_DIST = $(techcommoncomp_DATA) add-style.scm
