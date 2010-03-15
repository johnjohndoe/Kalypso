/****************************************************************************
*             READ_CFG .CPP  ->   WSP.CFG lesen                             *
*             10.03.1995                                                    *
****************************************************************************/

#ifndef _READ_CFG_H_INCLUDED_
#define _READ_CFG_H_INCLUDED_

int read_cfg_dat(); // util, wspd135, wspm001
int safe_cfg_dat(); // dito, wspd136
void lese_projektdatei(); // wspwin
void save_projekt(); // wspd147, d148, wspm001
void min_max_in_cfg_list(); // wspd136
void FillProbezList( SLIST* pProj_list, SLIST* pProbezlist ); // wspd148, wspd147

#endif _READ_CFG_H_INCLUDED_