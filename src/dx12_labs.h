#pragma once

#ifndef UNICODE
#define UNICODE
#endif

#include <D3Dcompiler.h>
#include <DirectXMath.h>
#include <Windows.h>
#include <dxgi1_4.h>
#include <initguid.h>
#include <wrl.h>

#include <exception>
#include <iostream>

#include <d3d12.h>
#include <string>
#include "d3dx12.h"

using namespace Microsoft::WRL;

namespace DX
{
    class com_exception : public std::exception
    {
    public:
        com_exception(HRESULT hr) : result(hr)
        {
            what_str = std::wstring(L"Failure with HRESULT of " +
                                    std::to_wstring(static_cast<unsigned int>(result)) +
                                    L"\n");
        }

        const LPCWSTR get_wstring() const { return what_str.c_str(); }

    private:
        HRESULT result;
        std::wstring what_str;
    };

    inline void ThrowIfFailed(HRESULT hr)
    {
        if (FAILED(hr))
        {
            throw com_exception(hr);
        }
    }
} // namespace DX

using namespace DX;
using namespace DirectX;

struct ColorVertex
{
    XMFLOAT3 position;
    XMFLOAT4 color;
};
