// Loss.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef LOSS_H
#define LOSS_H

#define VL_EINLAUF		0
#define VL_KRUEMMER		1
#define VL_ZUSATZ		2
#define VL_RECHEN		3
#define VL_AUSLAUF		4

#define N_VLTYPES		5

class Project;
class State;

class Loss : public CObject
{
public:
   Loss::Loss(Project *pProject, State* pState);
	 ~Loss();

   Loss* Clone();

	 double GetStation();
	 int GetType(int n);
	 double GetValue(int n);

	 void SetStation(double station);
	 void SetType(int n, int type);
	 void SetValue(int n, double value);
	 BOOL ValueDefined(int n);
	 void RemoveValue(int n);

protected:
	State *m_pState;
	Project *m_pProject;
	double m_dStation;
	int m_nType[N_VLTYPES];
	double m_dValue[N_VLTYPES];
};

#endif // LOSS_H