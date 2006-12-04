package org.kalypso.afgui.viz;

import org.eclipse.draw2d.Button;
import org.eclipse.draw2d.ColorConstants;
import org.eclipse.draw2d.Figure;
import org.eclipse.draw2d.Label;
import org.eclipse.draw2d.LineBorder;
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
	
	//final private Label label;
	final private IPhase phase;
	
	public PhaseFig(IPhase phase)
	{
//		super(phase.getName());
		//label= new Label(phase.getName());
		//add(label);
		this.phase=phase;
		setBorder(new LineBorder(ColorConstants.black,1));
		setOpaque(true);
		
//		TODO change api use phase.getActivities();
//		IActivity[] activities=new Activity[0];
//		for(IActivity act:activities)
//		{
//			add(new ActivityFigure(act));
//		}
		
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
