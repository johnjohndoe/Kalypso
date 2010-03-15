// DXFBlock.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef DXFBLOCK_H
#define DXFBLOCK_H

class CDXFLayer;
class CDXFText;
class CDXFLinie;
class CDXFKreis;
class CDXFPolylinie;
class CDXFArc;
class CDXFPunkt;

class CDXFBlock : public CObject
{
public:
	friend ostream& operator<<(ostream& os, CDXFBlock &bl);
	friend istream& operator>>(istream& is, CDXFBlock &bl);
	CDXFBlock(CDXFLayer* pOwner = NULL);
	~CDXFBlock();

	void AddText(CDXFText* tx);
	void AddLinie(CDXFLinie* ln);
	void AddPolylinie(CDXFPolylinie* pl);
	void AddKreis(CDXFKreis* kr);
	void AddArc(CDXFArc* ar);
	void AddPunkt(CDXFPunkt* pt);
	void DrawLinie( const double anfX, const double anfY, const double endX, const double endY );
	void DrawCross( const double posX, const double posY, const double rad );
	void DrawKreis( const double posX, const double posY, const double rad );
	void DrawPunkt( const double posX, const double posY );
	void DrawRect(double x, double y, double h, double w, double th);
	void SetName(CString& name) { m_name = name; }
	void GetName(CString& name) { name = m_name; }
	CDXFLayer* GetOwner() { return m_pOwner; }

protected:
	CDXFLayer* m_pOwner;
	CString m_name;
	double m_dX, m_dY, m_dZ;
	CTypedPtrList<CObList, CDXFText*> m_text;
	CTypedPtrList<CObList, CDXFLinie*> m_linien;
	CTypedPtrList<CObList, CDXFPolylinie*> m_polylinien;
	CTypedPtrList<CObList, CDXFKreis*> m_kreise;
	CTypedPtrList<CObList, CDXFArc*> m_arcs;
	CTypedPtrList<CObList, CDXFPunkt*> m_punkte;
};

#endif // DXFBLOCK_H