package org.kalypso.afgui.viz;

import org.eclipse.draw2d.Button;
import org.kalypso.afgui.model.IActivity;

public class ActivityFigure extends Button
{
	final private IActivity activity;
	
	public ActivityFigure(IActivity activity)
	{
		super(activity.getName());
		this.activity=activity;
	}
	
	public IActivity getActivity()
	{
		return activity;
	}
}
