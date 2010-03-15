// ProfileDistancer.h: Schnittstelle für die Klasse CProfileDistancer.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_PROFILEDISTANCER_H__E10560D1_D360_11D9_9697_000C29C56F8A__INCLUDED_)
#define AFX_PROFILEDISTANCER_H__E10560D1_D360_11D9_9697_000C29C56F8A__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "profilModel.h"

class Connection;
class CMapObject;
class CLayerArray;

class CProfileDistancer  
{
private:
	class distance_not_found
	{
	private:
		const CString m_message;
	public:
		distance_not_found( const CString& message ) : m_message( message ) {};
		const CString& GetMessage() const { return m_message; };
		LPCTSTR CTSTR() { return (LPCTSTR)m_message; };
	};

public:
	typedef std::map<CProfilModel::Zone, double> DistMap;
	typedef std::map<Connection*, DistMap*> ConnMap;

private:
	std::ostream& m_logStream;
	ConnMap m_connMap;

public:
	CProfileDistancer( std::ostream & logStream );
	virtual ~CProfileDistancer();

public:
	void AddDistances( Connection* conn, const long lineID, LPDISPATCH lineDisp, CMapObject* anfProfil, CMapObject* endProfil, CProfilModel* anfModel, CProfilModel* endModel, CLayerArray* layers );
	const ConnMap& GetConnMap() { return m_connMap; };

private:
	static double FindDistance( LPDISPATCH lineDisp, LPDISPATCH anfDisp, LPDISPATCH endDisp );
	void AddDistance( Connection* conn, const CProfilModel::Zone& zone, double distance );
};

#endif // !defined(AFX_PROFILEDISTANCER_H__E10560D1_D360_11D9_9697_000C29C56F8A__INCLUDED_)
