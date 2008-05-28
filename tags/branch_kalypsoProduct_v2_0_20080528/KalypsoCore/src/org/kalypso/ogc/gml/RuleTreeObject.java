/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml;

import java.awt.Rectangle;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.kalypso.contribs.eclipse.jface.viewers.ITooltipProvider;
import org.kalypso.core.i18n.Messages;
import org.kalypsodeegree.graphics.sld.Rule;

public class RuleTreeObject implements IWorkbenchAdapter, ITooltipProvider
{
  private static Object[] EMPTY_CHILDREN = new Object[] {};

  private Rule m_rule = null;

  private ThemeStyleTreeObject m_parent = null;

  public RuleTreeObject( Rule rule, ThemeStyleTreeObject parent )
  {
    m_rule = rule;
    m_parent = parent;
  }

  public RuleTreeObject( Object ruleObject, ThemeStyleTreeObject parent )
  {
    // can be either a simple Rule or a collection of Pattern-Rules
    if( ruleObject instanceof Rule )
      m_rule = (Rule) ruleObject;

// // in case of pattern rules, just take the first rule for labeling the
// // outline view
// else if( ruleObject instanceof RuleCollection && ( (RuleCollection)ruleObject ).size() > 0 )
// {
// m_rule = ( (RuleCollection)ruleObject ).get( 0 );
// }
    m_parent = parent;
  }

  public Rule getRule( )
  {
    return m_rule;
  }

  @Override
  public String toString( )
  {
    if( m_rule == null )
      return "<no styles set>"; //$NON-NLS-1$

    if( m_rule.getTitle() != null )
      return m_rule.getTitle();
    else if( m_rule.getName() != null )
      return m_rule.getName();
    else
      return "rule"; //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getChildren(java.lang.Object)
   */
  public Object[] getChildren( Object o )
  {
    return EMPTY_CHILDREN;
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getImageDescriptor(java.lang.Object)
   */
  public ImageDescriptor getImageDescriptor( Object object )
  {
    if( object != this )
      throw new IllegalStateException();

    if( m_rule == null )
      return null;

    /* Get the size of the symbol. It may be 0 / 0. */
    Rectangle size = RulePainter.getSize( getRule() );

    /* The default size. */
    int width = 16;
    int height = 16;

    /* Adjust if neccessary. */
    if( size != null && size.width > 0 )
      width = size.width;

    /* Adjust if neccessary. */
    if( size != null && size.height > 0 )
      height = size.height;

    /*
     * Draw the image on the fly to avoid the need to dispose it later. This is probably ok, because we wont have too
     * many RuleTreeObjects.
     */
    Display display = Display.getCurrent();
    Image image = new Image( display, width, height );
    GC gc = new GC( image );

    try
    {
      gc.setAntialias( SWT.ON );
      RulePainter.paint( m_rule, gc );

      /* No need to resize. */
      if( width == 16 && height == 16 )
      {
        ImageData imageData = image.getImageData();
        return ImageDescriptor.createFromImageData( imageData );
      }

      /* Resize, if the image is not 16 / 16. */
      Image resize = resize( image, 16, 16 );
      ImageData imageData = resize.getImageData();

      return ImageDescriptor.createFromImageData( imageData );
    }
    catch( Throwable t )
    {
      t.printStackTrace();

      return null;
    }
    finally
    {
      gc.dispose();
      image.dispose();
    }
  }

  /**
   * This function resizes the given image.
   * 
   * @param image
   *            The old image.
   * @param witdth
   *            The new width.
   * @param height
   *            The new height.
   */
  private Image resize( Image image, int width, int height )
  {
    Image scaled = new Image( image.getDevice(), width, height );
    GC gc = new GC( scaled );
    gc.setAntialias( SWT.ON );
    gc.setInterpolation( SWT.HIGH );
    gc.drawImage( image, 0, 0, image.getBounds().width, image.getBounds().height, 0, 0, width, height );
    gc.dispose();
    image.dispose(); // don't forget about me!
    return scaled;
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getLabel(java.lang.Object)
   */
  public String getLabel( Object o )
  {
    if( o != this )
      throw new IllegalStateException();

    if( m_rule == null )
      return "<no styles set>"; //$NON-NLS-1$

    if( m_rule.getTitle() != null )
      return m_rule.getTitle();

    if( m_rule.getName() != null )
      return m_rule.getName();

    return Messages.getString("org.kalypso.ogc.gml.RuleTreeObject.3"); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getParent(java.lang.Object)
   */
  public Object getParent( Object o )
  {
    if( o != this )
      throw new IllegalStateException();

    return m_parent;
  }

  public KalypsoUserStyle getStyle( )
  {
    return m_parent.getStyle();
  }

  public IKalypsoFeatureTheme getTheme( )
  {
    return m_parent.getTheme();
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.viewers.ITooltipProvider#getTooltip(java.lang.Object)
   */
  public String getTooltip( Object element )
  {
    if( element != this )
      throw new IllegalStateException();

    return m_rule.getAbstract();
  }
}