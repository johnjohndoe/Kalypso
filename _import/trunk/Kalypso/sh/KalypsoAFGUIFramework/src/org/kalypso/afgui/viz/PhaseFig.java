package org.kalypso.afgui.viz;

import org.eclipse.draw2d.Button;
import org.eclipse.draw2d.ButtonBorder;
import org.eclipse.draw2d.Clickable;
import org.eclipse.draw2d.ColorConstants;
import org.eclipse.draw2d.Figure;
import org.eclipse.draw2d.FlowLayout;
import org.eclipse.draw2d.Graphics;
import org.eclipse.draw2d.Label;
import org.eclipse.draw2d.LineBorder;
import org.eclipse.draw2d.RoundedRectangle;
import org.eclipse.draw2d.geometry.Dimension;
import org.eclipse.swt.graphics.Color;
import org.kalypso.afgui.model.IActivity;
import org.kalypso.afgui.model.IPhase;
import org.kalypso.afgui.model.impl.Activity;

/**
 * Used to draw a phase as a rounded rectangle
 *  
 * @author Patrice Congo
 *
 */
public class PhaseFig extends Clickable//RoundedRectangle
{
	public static Color color = new Color(null,255,255,106);
	
	final private IPhase phase;
	
	public PhaseFig(IPhase phase)
	{
		super(new Label(phase.getName()), Clickable.STYLE_BUTTON);
		Label label= new Label(phase.getName());
		
		//add(label);
		this.phase=phase;
		setLayoutManager(new FlowLayout());
		//setBorder(new LineBorder(ColorConstants.tooltipBackground,3));
		setBorder(new ButtonBorder());
		setOpaque(false);
		//setMinimumSize(new Dimension(100,20));
		//setCornerDimensions(new Dimension(10,8));
	}
	
//	@Override
//	protected void fillShape(Graphics graphics)
//	{
//		super.fillShape(graphics);
//		
//	}
	
	/**
	 * Return the phase which is beeing represented
	 * @return
	 */
	public IPhase getPhase()
	{
		return phase;
	}
	
}
