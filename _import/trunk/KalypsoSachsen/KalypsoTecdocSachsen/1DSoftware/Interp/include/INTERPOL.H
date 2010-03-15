// Interpol.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef INTERPOL_H
#define INTERPOL_H

#define VORLAND_LINKS	0
#define FLUSS			1
#define VORLAND_RECHTS	2

class Batch;

class Interpolation : public CObject
{
public:
	Interpolation(Project* pProject);
	~Interpolation();

	BOOL Initialize(CrossSection* src1, CrossSection* src2);
	BOOL Interpolate(CrossSection* cs);
	BOOL DoProfilShift(CrossSection* cs, Batch& batch, int i);
	void DeleteContents();

protected:
	Project *m_pProject;
	CrossSection *m_pCS[2];		// source cross sections
	double m_dSrcSt[2];			// stations of source cross sections
	Profil *m_pSrc[2];			// source profils (loaded from source cross sections)
	Profil *m_pSrcSec[2][3];	// split sections of source profils (Vorlandlinks, Fluss, Vorlandrechts)
	double m_xmin[2][3];		// min x values of split sections
	double m_xmax[2][3];		// max x values of split sections
	double m_ymin[2][3];		// min y values of split sections
//	double m_orgxmin[2];		// original min x values of source profils

	CrossSection *m_pNew;		// new cross section (interpolated)
	double m_dNewSt;			// station of new cross section
	Profil *m_pNewSec[3];		// split sections of new cross section

	BOOL ShiftProfil(Profil* prof, BOOL bLowPoint, double factor = 0, BOOL bErrors = FALSE);
	BOOL SplitSources();
	BOOL SplitProfil(Profil* prof);
	BOOL ExtractSection(int nSrc, int nType);
	void SetSectionLimits(int nSrc, int nType);
	void UnifySections(int nType);
	void CreateNewSections();
	BOOL InterpolateNewSections();
	void CombineNewSections();
	BOOL LimitPointCount();
	void VertShift(Profil* prof, double shift);
};

/*************************** INTERPOLATION **************************/
/*
The interpolation is carried out as follows:
1. Initialize
	a) Set variables and shift each complete profil until the lowest point has
	x coordinate equal to zero.
	b) Split each complete profil into 3 profil sections:
	Vorlandlinks (everything left of the trennflaechen), Fluss (everything
	between the trennflaechen), Vorlandrechts (everything right of trennflaechen).
	When trennflaechen exist, Vorlandlinks and Vorlandrechts include the points for
	the trennflaechen themselves. Otherwise they don't have any points.
	c) Shift each section until lowest point is zero (Fluss), right most point
	is zero (Vorlandlinks), or left most point is zero (Vorlandrechts).
	d) Calculate min and max x-values for each section.
	e) For each section find max of min x-values and min of max x-values. In this
	range take each section pair and ensure they have the same x-value points (i.e.
	take the union of the x-value points of two sections)
2. Interpolate
	a) Create three new profil sections with points equal to the points in the common
	range of the original sections.
	b) Interpolate the heights of each point in the common range. When points in the
	original sections lie outside the common range, create new points in the new sections
	and use the same gradients as the original section.
	c) Combine each new section to create a complete new profil.
*/

#endif // INTERPOL_H