// DXFLayer.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef DXFLAYER_H
#define DXFLAYER_H

class CDXFText;
class CDXFLinie;
class CDXFKreis;
class CDXFArc;
class CDXFPunkt;
class CDXFPolylinie;
class CDXFBlock;
class CDXFBlockInsert;
class CDXFZeichnung;

#define DXF_TYPE_BLOCK	0
#define DXF_TYPE_ENTITY	1

#define WSIZE_1		1
#define WSIZE_2		2
#define WSIZE_3		3
#define WSIZE_4		4
#define WSIZE_5		5
#define WSIZE_6		6
#define WSIZE_7		7
#define WSIZE_8		8
#define WSIZE_9		9
#define WSIZE_10	10
#define WSIZE_12	12
#define WSIZE_13	13
#define WSIZE_20	20

class CDXFLayer : public CObject
{
public:
	friend ostream& operator<<(ostream& os, CDXFLayer &ly);
	CDXFLayer(CDXFZeichnung* pOwner = NULL);
	~CDXFLayer();

	void SetName( const CString& name) { m_name = name; }
	void GetName( CString& name) { name = m_name; }
	int GetColor();
	void GetLType(CString& str);
	void AddText(CDXFText* tx);
	void AddLinie(CDXFLinie* ln);
	void AddKreis(CDXFKreis* kr);
	void AddArc(CDXFArc* ar);
	void AddPunkt(CDXFPunkt* pt);
	void AddPolylinie(CDXFPolylinie* pl);
	void AddBlock(CDXFBlock* bl);
	void AddBlockInsert(CDXFBlockInsert* bi);
	void DrawLinie( const double anfX, const double anfY, const double endX, const double endY );
	void DrawKreis( const double posX, const double posY, const double rad );
	void DrawPunkt( const double posX, const double posY );
	void DrawFrame( double gxx, double gyx );
	void DrawGrid( double scale, double rseite, int anzy, int anzx );
	void DrawCross( const double posX, const double posY, const double rad);
	CDXFZeichnung* GetOwner() { return m_pOwner; }

	int m_outputType;
	int m_nColor;
	CString m_LType;

protected:
	CDXFZeichnung* m_pOwner;
	CString m_name;
	CTypedPtrList<CObList, CDXFText*> m_text;
	CTypedPtrList<CObList, CDXFLinie*> m_linien;
	CTypedPtrList<CObList, CDXFKreis*> m_kreise;
	CTypedPtrList<CObList, CDXFArc*> m_arcs;
	CTypedPtrList<CObList, CDXFPunkt*> m_punkte;
	CTypedPtrList<CObList, CDXFPolylinie*> m_polylinien;
	CTypedPtrList<CObList, CDXFBlock*> m_blocks;
	CTypedPtrList<CObList, CDXFBlockInsert*> m_inserts;
};

#endif // DXFLAYER_H