// DXFZeich.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef DXFZEICH_H
#define DXFZEICH_H

class CDXFBlock;
class CDXFLType;
class CDXFLayer;
class CDrawDoc;


class CDXFZeichnung : public CObject
{
public:
	friend ostream& operator<<(ostream& os, CDXFZeichnung &zn);
	friend istream& operator>>(istream& is, CDXFZeichnung &zn);
	CDXFZeichnung(CDrawDoc* pOwner);
	~CDXFZeichnung();

	void LoadHeader(istream& is);
	void LoadBlocks(istream& is);
	void LoadEntities(istream& is);

	void SetSize(double xb, double yb, double xo, double yo);
	void SetParams(double sc, double rs, int k, int i, double xa, double ya);
	void SetText(CString& kpfpath, CString& CDXFpath);
	double GetXBlatt() { return xblatt; }
	double GetYBlatt() { return yblatt; }
	double GetXOff() { return xxof; }
	double GetYOff() { return yyof; }
	double GetXAdd() { return xadd; }
	double GetYAdd() { return yadd; }
	void SetLayer( const CString& name, CDXFLayer* ly );
	CDXFLayer* FindLayer( const CString& name );
	void AddLType(CDXFLType* lt);
	void SetDefaultLTypes();

	CString m_kpfpath, m_CDXFpath;
	CTypedPtrList<CObList, CDXFLType*> m_ltypes;

protected:
	CDrawDoc* m_pOwner;
	CDXFBlock* m_pLastBlockLoaded;
	CTypedPtrMap<CMapStringToOb, CString, CDXFLayer*> m_layers;
	double xblatt, yblatt, xxof, yyof;
	double scale, rseite, xadd, yadd;
	int km, im;
};

#endif // DXFZEICH_H