package org.kalypso.eclipse.jface.action;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.resource.ImageDescriptor;

/**
 * Action with full Constructor.
 * 
 * @author gernot
 */
public class FullAction extends Action
{
  public FullAction( String text, ImageDescriptor image, String tooltipText )
  {
    super( text, image );
    
    setToolTipText( tooltipText );
  }
}