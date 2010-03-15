// 3DCoord.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef C3DCOORD_H
#define C3DCOORD_H

class C3DCoord : public CObject
{
public:
   C3DCoord::C3DCoord();
   C3DCoord::C3DCoord(const C3DCoord& crd);
	~C3DCoord();

	 C3DCoord& operator= (const C3DCoord &other);
	 BOOL operator==(C3DCoord crd) const;
	 BOOL operator!=(C3DCoord crd) const;

	 void Set(double x, double y, double z);

	double dx;
	double dy;
	double dz;
};

#endif // C3DCOORD_H