#include "stdafx.h"

#include "state.h"

#include "loss.h"

  ////////////////////////////
  //  Klasse  Loss
  ///////////////////////////

/* The Default Constructor */
Loss::Loss(Project* pProject, State* pState)
{
	m_pProject = pProject;
	m_pState = pState;
	m_dStation = 0;
	for (int i=0; i<N_VLTYPES; i++)
	{
		m_nType[i] = i;
    m_dValue[i] = std::numeric_limits<double>::infinity();
	}
}

Loss::~Loss()
{
}

Loss* Loss::Clone()
{
  Loss* loss = new Loss( m_pProject, m_pState );
  loss->SetStation( m_dStation );
  for ( int i = 0; i < N_VLTYPES; i++ )
  {
    loss->SetValue( i, m_dValue[i] );
    loss->SetType( i, m_nType[i] );
  };

  return loss;
}; // Clone

double Loss::GetStation()
{
	return m_dStation;
}

int Loss::GetType(int n)
{
	if (n>=0 && n<N_VLTYPES)
		return m_nType[n];
	
	return -1;
}

double Loss::GetValue(int n)
{
	if (n>=0 && n<N_VLTYPES)
		return m_dValue[n];

	return std::numeric_limits<double>::infinity();
}

void Loss::SetStation(double station)
{
	m_dStation = station;
	if (m_pState!=NULL)
		m_pState->SetLossModified();
}

void Loss::SetType(int n, int type)
{
	if (n>=0 && n<N_VLTYPES && type>=0 && type<N_VLTYPES)
		m_nType[n] = type;
	if (m_pState!=NULL)
		m_pState->SetLossModified();
}

void Loss::SetValue(int n, double value)
{
	if (n>=0 && n<N_VLTYPES)
		m_dValue[n] = value;
	if (m_pState!=NULL)
		m_pState->SetLossModified();
}

BOOL Loss::ValueDefined(int n)
{
	if (n>=0 && n<N_VLTYPES)
    return m_dValue[n] != std::numeric_limits<double>::infinity();

	return FALSE;
}

void Loss::RemoveValue(int n)
{
	if (n>=0 && n<N_VLTYPES)
    m_dValue[n] = std::numeric_limits<double>::infinity();
	if (m_pState!=NULL)
		m_pState->SetLossModified();
}