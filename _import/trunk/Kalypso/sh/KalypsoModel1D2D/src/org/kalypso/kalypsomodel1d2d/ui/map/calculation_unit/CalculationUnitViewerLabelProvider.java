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
package org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit2D;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

/**
 * @author Madanagopal
 */
public class CalculationUnitViewerLabelProvider extends LabelProvider
{
  private final Display m_display;

  private final Image calc1DImage;

  private final Image calc2DImage;

  private final Image calc1D2DImage;

  public CalculationUnitViewerLabelProvider( final Display display )
  {
    m_display = display;

    // TODO: these images never get disposed!

    calc1DImage = new Image( m_display, KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/1d_Element.GIF" ).getImageData() ); //$NON-NLS-1$

    calc2DImage = new Image( m_display, KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/2d_Element.GIF" ).getImageData() ); //$NON-NLS-1$

    calc1D2DImage = new Image( m_display, KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/1d_2d_Element.GIF" ).getImageData() ); //$NON-NLS-1$
  }

  @Override
  public Image getImage( final Object element )
  {
    if( element instanceof ICalculationUnit1D )
      return calc1DImage;

    if( element instanceof ICalculationUnit2D )
      return calc2DImage;
    if( element instanceof ICalculationUnit1D2D )
      return calc1D2DImage;

    throw new UnsupportedOperationException( "Only IFeatureWrapper2 is supported:" + "but got \n\tclass=" + (element == null ? null : element.getClass()) + "\n\t value=" + element ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  }

  @Override
  public String getText( final Object element )
  {
    if( element instanceof IFeatureWrapper2 )
    {
      final String name = ((IFeatureWrapper2) element).getName();
      if( name != null )
        return name;

      return ((IFeatureWrapper2) element).getGmlID();
    }

    throw new UnsupportedOperationException( "Only IFeatureWrapper2 is supported:" + "but got \n\tclass=" + (element == null ? null : element.getClass()) + "\n\t value=" + element ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  }

}
