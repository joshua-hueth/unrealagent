// Copyright Epic Games, Inc. All Rights Reserved.

#include "SceneRendering.h"
#include "RenderCore.h"
#include "TranslucentRendering.h"

void FMobileSceneRenderer::RenderTranslucency(FRHICommandList& RHICmdList, const FViewInfo& View)
{	
	const bool bShouldRenderTranslucency = ShouldRenderTranslucency(StandardTranslucencyPass) && ViewFamily.EngineShowFlags.Translucency;
	if (bShouldRenderTranslucency)
	{
		CSV_SCOPED_TIMING_STAT_EXCLUSIVE(RenderTranslucency);
		SCOPE_CYCLE_COUNTER(STAT_TranslucencyDrawTime);

		RHI_BREADCRUMB_EVENT_STAT(RHICmdList, Translucency, "Translucency");
		SCOPED_GPU_STAT(RHICmdList, Translucency);

		// BEGIN META SECTION - Multi-View Per View Viewports / Render Areas
		SetStereoViewport(RHICmdList, View);
		// END META SECTION - Multi-View Per View Viewports / Render Areas
		View.ParallelMeshDrawCommandPasses[StandardTranslucencyMeshPass].Draw(RHICmdList, &TranslucencyInstanceCullingDrawParams);
	}
}
