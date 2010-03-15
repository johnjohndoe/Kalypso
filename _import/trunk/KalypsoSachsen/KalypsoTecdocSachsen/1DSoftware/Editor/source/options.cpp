// options.cpp : implementation file
//
#include "stdafx.h"

#include "global.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CUnit

const CUnit& CUnit::operator=(const CUnit& unit)
{
	m_nTPU = unit.m_nTPU;
	m_nSmallDiv = unit.m_nSmallDiv;
	m_nMediumDiv = unit.m_nMediumDiv;
	m_nLargeDiv = unit.m_nLargeDiv;
	m_nMinMove = unit.m_nMinMove;
	m_nAbbrevID = unit.m_nAbbrevID;
	m_bSpaceAbbrev = unit.m_bSpaceAbbrev;
	m_strAbbrev = unit.m_strAbbrev;
	return *this;
}

CUnit::CUnit(int nTPU, int nSmallDiv, int nMediumDiv, int nLargeDiv, 
		int nMinMove, UINT nAbbrevID, BOOL bSpaceAbbrev)
{
	m_nTPU = nTPU;
	m_nSmallDiv = nSmallDiv;
	m_nMediumDiv = nMediumDiv;
	m_nLargeDiv = nLargeDiv;
	m_nMinMove = nMinMove;
	m_nAbbrevID = nAbbrevID;
	m_bSpaceAbbrev = bSpaceAbbrev;
}
