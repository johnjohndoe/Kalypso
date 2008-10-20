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
import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.kalypso.contribs.eclipse.jface.viewers.ITooltipProvider;
import org.kalypso.core.i18n.Messages;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.Symbolizer;

public class RuleTreeObject implements IWorkbenchAdapter, ITooltipProvider
{
  private Rule m_rule = null;

  private ThemeStyleTreeObject m_parent = null;

  public RuleTreeObject( final Rule rule, final ThemeStyleTreeObject parent )
  {
    m_rule = rule;
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
  public Object[] getChildren( final Object o )
  {
    // We collect all children of the symbolisers,
    // as we do not want to show the symbolisers (they got painted
    // as a combined symbol for this tree object)
    // Instead we return the collection of all their children
    final List<Object> result = new ArrayList<Object>();
    final Symbolizer[] symbolizers = m_rule.getSymbolizers();
    for( final Symbolizer symbolizer : symbolizers )
    {
      final SymbolizerTreeObject symbTreeObject = SymbolizerTreeObject.create( this, symbolizer );
      final Object[] children = symbTreeObject.getChildren( symbTreeObject );
      for( final Object child : children )
        result.add( child );
    }

    return result.toArray( new Object[result.size()] );
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getImageDescriptor(java.lang.Object)
   */
  public ImageDescriptor getImageDescriptor( final Object object )
  {
    if( object != this )
      throw new IllegalStateException();

    if( m_rule == null )
      return null;

    /* Get the size of the symbol. It may be 0 / 0. */
    final Rectangle size = RulePainter.getSize( getRule() );

    /* The default size. */
    int width = 16;
    int height = 16;

    /* Adjust if neccessary. */
    if( size != null && size.width > 0 )
      width = size.width;

    /* Adjust if neccessary. */
    if( size != null && size.height > 0 )
      height = size.height;

    final TreeObjectImage treeImage = new TreeObjectImage( width, height );

    /*
     * Draw the image on the fly to avoid the need to dispose it later. This is probably ok, because we wont have too
     * many RuleTreeObjects.
     */

    try
    {
      RulePainter.paint( m_rule, treeImage.getGC() );

      return treeImage.getImageDescriptor();
    }
    catch( final Throwable t )
    {
      t.printStackTrace();

      return null;
    }
    finally
    {
      treeImage.dispose();
    }
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getLabel(java.lang.Object)
   */
  public String getLabel( final Object o )
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
  public Object getParent( final Object o )
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
  public String getTooltip( final Object element )
  {
    if( element != this )
      throw new IllegalStateException();

    return m_rule.getAbstract();
  }
}