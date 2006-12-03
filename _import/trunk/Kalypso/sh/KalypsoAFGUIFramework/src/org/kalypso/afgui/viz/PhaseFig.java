package org.kalypso.afgui.viz;

import org.eclipse.draw2d.Label;
import org.eclipse.draw2d.RoundedRectangle;
import org.kalypso.afgui.model.IActivity;
import org.kalypso.afgui.model.IPhase;
import org.kalypso.afgui.model.impl.Activity;

/**
 * Used to draw a phase as a rounded rectangle
 *  
 * @author Patrice Congo
 *
 */
public class PhaseFig extends RoundedRectangle
{
	
	final private Label label;
	final private IPhase phase;
	
	public PhaseFig(IPhase phase)
	{
		super();
		label= new Label(phase.getName());
		add(label);
		this.phase=phase;
		IActivity[] activities=new Activity[0];//TODO change api use phase.getActivities();
		for(IActivity act:activities)
		{
			add(new ActivityFigure(act));
		}
		
	}
	
	/**
	 * Return the phase which is beeing represented
	 * @return
	 */
	public IPhase getPhase()
	{
		return phase;
	}
	
}
