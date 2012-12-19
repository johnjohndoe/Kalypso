/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.kalypsomodel1d2d.ui.calculationUnitView;

import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit2D;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class CalcUnitNameLabelProvider extends ColumnLabelProvider
{
  private final Image m_calc1DImage;

  private final Image m_calc2DImage;

  private final Image m_calc1D2DImage;

  public CalcUnitNameLabelProvider( final Display display )
  {
    m_calc1DImage = new Image( display, KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/1d_Element.GIF" ).getImageData() ); //$NON-NLS-1$
    m_calc2DImage = new Image( display, KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/2d_Element.GIF" ).getImageData() ); //$NON-NLS-1$
    m_calc1D2DImage = new Image( display, KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/1d_2d_Element.GIF" ).getImageData() ); //$NON-NLS-1$
  }

  @Override
  public void dispose( )
  {
    super.dispose();

    m_calc1DImage.dispose();
    m_calc2DImage.dispose();
    m_calc1D2DImage.dispose();
  }

  @Override
  public Image getImage( final Object element )
  {
    if( element instanceof ICalculationUnit1D )
      return m_calc1DImage;

    if( element instanceof ICalculationUnit2D )
      return m_calc2DImage;
    if( element instanceof ICalculationUnit1D2D )
      return m_calc1D2DImage;

    throw new UnsupportedOperationException( "Only Feature is supported:" + "but got \n\tclass=" + (element == null ? null : element.getClass()) + "\n\t value=" + element ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  }

  @Override
  public String getText( final Object element )
  {
    if( element instanceof Feature )
    {
      final String name = ((Feature)element).getName();
      if( name != null )
        return name;

      return ((Feature)element).getId();
    }

    throw new UnsupportedOperationException( "Only Feature is supported:" + "but got \n\tclass=" + (element == null ? null : element.getClass()) + "\n\t value=" + element ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  }
}