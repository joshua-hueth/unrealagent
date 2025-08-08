// Copyright Epic Games, Inc. All Rights Reserved.

#pragma once

#include "CoreMinimal.h"
#include "IStereoLayers.h"
#include "OpenXRCore.h"
#include "XRSwapChain.h"

union FXrCompositionLayerUnion;

struct FOpenXRLayer
{
	struct FPerEyeTextureData
	{
		FXRSwapChainPtr			Swapchain = nullptr;
		FVector2D				SwapchainSize{};
// BEGIN META SECTION - Add XR Cubemap Support
		ETextureDimension		SwapchainDimension{};
		bool					bUpdateTexture = false;

		void					SetSwapchain(FXRSwapChainPtr InSwapchain, const FVector2D& InSwapchainSize, ETextureDimension InSwapchainDimension)
		{
			Swapchain = InSwapchain;
			SwapchainSize = InSwapchainSize;
			SwapchainDimension = InSwapchainDimension;
			bUpdateTexture = true;
		}
// BEGIN META SECTION - Add XR Cubemap Support
	};

	IStereoLayers::FLayerDesc	Desc;

	/** Texture tracking data for the right eye.*/
	FPerEyeTextureData			RightEye;

	/** Texture tracking data for the left eye, may not be present.*/
	FPerEyeTextureData			LeftEye;

	FOpenXRLayer(const IStereoLayers::FLayerDesc& InLayerDesc)
		: Desc(InLayerDesc)
	{ }

	void SetLayerId(uint32 InId) { Desc.SetLayerId(InId); }
	uint32 GetLayerId() const { return Desc.GetLayerId(); }
// BEGIN META SECTION - Add XR Cubemap Support
	bool NeedReallocateRightTexture(ETextureDimension TextureDimension);
	bool NeedReallocateLeftTexture(ETextureDimension TextureDimension);
// END META SECTION - Add XR Cubemap Support
	FIntRect GetRightViewportSize() const;
	FVector2D GetRightQuadSize() const;

	FIntRect GetLeftViewportSize() const;
	FVector2D GetLeftQuadSize() const;

// BEGIN META SECTION - Add Depth Test 
	TArray<FXrCompositionLayerUnion> CreateOpenXRLayer(FTransform InvTrackingToWorld, float WorldToMeters, XrSpace Space);
// END META SECTION - Add Depth Test 
	
private:

// BEGIN META SECTION - Add Depth Test 
	TArray<XrCompositionLayerDepthTestFB> CompositionLayerDepthTest;
// END META SECTION - Add Depth Test 

	void CreateOpenXRQuadLayer(bool bIsStereo, bool bNoAlpha, FTransform PositionTransform, float WorldToMeters, XrSpace Space, TArray<FXrCompositionLayerUnion>& Headers) const;
	void CreateOpenXRCylinderLayer(bool bIsStereo, bool bNoAlpha, FTransform PositionTransform, float WorldToMeters, XrSpace Space, TArray<FXrCompositionLayerUnion>& Headers) const;
	void CreateOpenXREquirectLayer(bool bIsStereo, bool bNoAlpha, FTransform PositionTransform, float WorldToMeters, XrSpace Space, TArray<FXrCompositionLayerUnion>& Headers) const;

// BEGIN META SECTION - Equirect2 
	void CreateOpenXREquirect2Layer(bool bIsStereo, bool bNoAlpha, FTransform PositionTransform, float WorldToMeters, XrSpace Space, TArray<FXrCompositionLayerUnion>& Headers) const;
	void SetupEquirect2(FVector2D Scale, FVector2D Bias, FVector2D Position, FVector2D Size, FTransform PositionTransform, float WorldToMeters, XrCompositionLayerEquirect2KHR& Equirect2) const;
// END META SECTION - Equirect2 


// BEGIN META SECTION - Add Cubemap 
	void CreateOpenXRCubemapLayer(bool bIsStereo, bool bNoAlpha, FTransform PositionTransform, float WorldToMeters, XrSpace Space, TArray<FXrCompositionLayerUnion>& Headers) const;
	bool NeedReallocateTexture(FPerEyeTextureData& EyeTextureData, FTextureRHIRef InTexture, ETextureDimension TextureDimension);
// END META SECTION - Add Cubemap 

};


bool GetLayerDescMember(const FOpenXRLayer& Layer, IStereoLayers::FLayerDesc& OutLayerDesc);
void SetLayerDescMember(FOpenXRLayer& OutLayer, const IStereoLayers::FLayerDesc& InLayerDesc);
void MarkLayerTextureForUpdate(FOpenXRLayer& Layer);
