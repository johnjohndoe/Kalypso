package org.kalypso.afgui.model.impl;

import org.kalypso.afgui.model.EActivityExeState;
import org.kalypso.afgui.model.IActivityRuntimeStatus;
import org.kalypso.afgui.schema.Schema;

import com.hp.hpl.jena.rdf.model.Resource;

public class ActivityRuntimeStatus implements IActivityRuntimeStatus
{
	private EActivityExeState exeState;
	
	private Resource runtimeStatus;
	
	public ActivityRuntimeStatus()
	{
		this(EActivityExeState.NOT_STARTED);
		
	}
	
	
	public ActivityRuntimeStatus(EActivityExeState exeState)
	{
		
		this.exeState=exeState;
		runtimeStatus.addProperty(Schema.PROP_EXE_STATE, exeState.toString());
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
