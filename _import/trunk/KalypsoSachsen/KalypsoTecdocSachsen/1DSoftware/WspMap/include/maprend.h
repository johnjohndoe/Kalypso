// MapRend.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef MAPREND_H
#define MAPREND_H

#define N_FIELDS	4
#define N_CONSTS	3
#define N_BOOLS		7
#define N_COLORS	2

class CMapRenderer : public CObject
{
	DECLARE_SERIAL(CMapRenderer);
public:
	CMapRenderer();
	CMapRenderer(const CMapRenderer& src);
	~CMapRenderer();

	virtual void Serialize(CArchive& ar);

	enum RendererType
	{
		singleSymbol,
		valueMap,
		classBreaks,
		standardLabel,
		advancedLabel
	};

// Attributes
public:
	void SetRendererType(int n);
	int GetRendererType() { return m_nRendType; }
	void SetField( const short n, const CString& val );
	CString GetField(short n);
	void SetConst(short n, short val);
	short GetConst(short n);
	void SetBool(short n, BOOL val);
	BOOL GetBool(short n);
	void SetColor(short n, COLORREF val);
	COLORREF GetColor(short n);
	void SetFont(LOGFONT logfont) { m_logfont = logfont; }
	LOGFONT GetFont() { return m_logfont; }
	void SetValueColors(CArray<COLORREF, COLORREF>& colors);
	COLORREF GetValueColor(short n);
	short GetNumValueColors();
	void ClearValueColors();

protected:
	RendererType m_nRendType;
	CString m_strField[N_FIELDS];
	//	**Renderer**:	ValueMap		ClassBreak		LabelRenderer	LabelPlacer
	//	***************************************************************************
	//	** Field0 **:	Field			    Field			  Field			    Field
	//	** Field1 **:	RotationField	N/A				  XOffsetField	N/A
	//	** Field2 **:	ScalingField	N/A				  YOffsetField	N/A
	//	** Field3 **:	N/A				    N/A				  FittedField		N/A
	short m_nConst[N_CONSTS];
	//	**Renderer**:	ValueMap		ClassBreak		LabelRenderer	LabelPlacer
	//	***************************************************************************
	//	** Const0 **:	N/A				  BreakCount		HorzAlign		  Height/Width
	//	** Const1 **:	N/A				  startSize		  VertAlign		  Size
	//	** Const2 **:	N/A				  endSize			  Rotation		  ScaleFactor
	BOOL m_bBool[N_BOOLS];
	//	**Renderer**:	ValueMap		ClassBreak		LabelRenderer	LabelPlacer
	//	***************************************************************************
	//	** Bool0  **:	RemoveOut.	RemoveOut     DrawBackground	DrawBackground
	//	** Bool1  **:	N/A				  N/A				    AllowDuplicates	AllowDuplicates
	//	** Bool2  **:	N/A				  N/A				    SplinedText		  Mask
	//	** Bool3  **:	N/A				  N/A				    Flip			      Scale
	//	** Bool4  **:	N/A				  N/A				    N/A				      PlaceOn
	//	** Bool5  **:	N/A				  N/A				    N/A				      PlaceAbove
	//	** Bool6  **:	N/A				  N/A				    N/A				      PlaceBelow
	COLORREF m_cColor[N_COLORS];
	//	**Renderer**:	ValueMap		ClassBreak		LabelRenderer	LabelPlacer
	//	***************************************************************************
	//	** Color0 **:	N/A				  StartColor		TextColor		TextColor
	//	** Color1 **:	N/A				  EndColor		  N/A				  MaskColor
	LOGFONT m_logfont;
	//	**Renderer**:	ValueMap		ClassBreak		LabelRenderer	LabelPlacer
	//	***************************************************************************
	//	** Logfont**:	N/A				N/A				TextFont		TextFont
	CArray<COLORREF, COLORREF> m_valCols;
	//	**Renderer**:	ValueMap		ClassBreak		LabelRenderer	LabelPlacer
	//	***************************************************************************
	//	** valCols**:	valCols			N/A				N/A				N/A
};

#endif // MAPREND_H