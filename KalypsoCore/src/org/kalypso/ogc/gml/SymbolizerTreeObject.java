/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 * 
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 * 
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ogc.gml;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.kalypso.contribs.eclipse.jface.viewers.ITooltipProvider;
import org.kalypso.ogc.gml.symbolizer.RasterSymbolizerTreeObject;
import org.kalypsodeegree.graphics.sld.RasterSymbolizer;
import org.kalypsodeegree.graphics.sld.Symbolizer;

/**
 * @author Gernot Belger
 */
public class SymbolizerTreeObject implements IWorkbenchAdapter, ITooltipProvider
{
  /**
   * Factory method for these tree objects.<br>
   * Should probably moved elsewhere, maybe even into an extension.point.
   */
  public static SymbolizerTreeObject create( final Object parent, final Symbolizer symbolizer )
  {
    if( symbolizer instanceof RasterSymbolizer )
      return new RasterSymbolizerTreeObject( parent, (RasterSymbolizer) symbolizer );

    return new SymbolizerTreeObject( parent, symbolizer );
  }

  private static Object[] EMPTY_CHILDREN = new Object[] {};

  private final Symbolizer m_symbolizer;

  private final Object m_parent;

  public SymbolizerTreeObject( final Object parent, final Symbolizer symbolizer )
  {
    m_parent = parent;
    m_symbolizer = symbolizer;
  }

  public Symbolizer getSymbolizer( )
  {
    return m_symbolizer;
  }

  /**
   * Default implementation returns no children. Overwrite for special behaviour.
   * 
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getChildren(java.lang.Object)
   */
  @Override
  public Object[] getChildren( final Object o )
  {
    return EMPTY_CHILDREN;
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getImageDescriptor(java.lang.Object)
   */
  @Override
  public ImageDescriptor getImageDescriptor( final Object object )
  {
    // Not yet supported; for the moment this code resided inside the RulePainter
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getLabel(java.lang.Object)
   */
  @Override
  public String getLabel( final Object o )
  {
    return null;
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getParent(java.lang.Object)
   */
  @Override
  public Object getParent( final Object o )
  {
    return m_parent;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.viewers.ITooltipProvider#getTooltip(java.lang.Object)
   */
  @Override
  public String getTooltip( final Object element )
  {
    return null;
  }

}
