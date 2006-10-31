package org.kalypso.afgui.model.impl;

import org.kalypso.afgui.model.EActivityExeState;
import org.kalypso.afgui.model.IActivityRuntimeStatus;

public class ActivityRuntimeStatus implements IActivityRuntimeStatus
{
	private EActivityExeState exeState;

	public ActivityRuntimeStatus()
	{
		this(EActivityExeState.NOT_STARTED);
	}
	
	public ActivityRuntimeStatus(EActivityExeState exeState)
	{
		this.exeState=exeState;
	}
	
	public EActivityExeState getExeState()
	{
		return exeState;
	}

	public void setExeState(EActivityExeState exeState)
	{
		this.exeState=exeState;

	}

}
