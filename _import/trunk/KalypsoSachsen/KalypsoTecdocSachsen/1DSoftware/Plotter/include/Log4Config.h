/*! Time-stamp: <@(#)Log4Config.h   17.01.03 - 10:56:40   Belger>
 *********************************************************************
 *  @file   : Log4Config.h
 *
 *  Author  : Belger                              Date: 17.01.03
 *
 *  Purpose : Declaration of class CLog4Config
 *
 *********************************************************************
 */
// Log4Config.h: Schnittstelle für die Klasse CLog4Config.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_LOG4CONFIG_H__CED29653_29F5_11D7_B375_00104BB3E525__INCLUDED_)
#define AFX_LOG4CONFIG_H__CED29653_29F5_11D7_B375_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

/*!
 * @class CLog4Config
 * Diese Klasse macht nix anderes, als die globalen Einstellungen für
 * das lgging einzustellen
 * Nur für Debug-Zwecke
 *
 * @usage
 *  Im Konstruktor werden alle benötigten Appender erstellt und den zur Zeit
 *  benutzten Kategoeiren zugeordnet. Das wars
 *  Die Klasse ist Singleton und stets vorhanden.
*/
class CLog4Config  
{
private:
	CLog4Config();

  static CLog4Config theLog4Config; // das einzig existierende CLog4ConfigObject
};

#endif // !defined(AFX_LOG4CONFIG_H__CED29653_29F5_11D7_B375_00104BB3E525__INCLUDED_)
