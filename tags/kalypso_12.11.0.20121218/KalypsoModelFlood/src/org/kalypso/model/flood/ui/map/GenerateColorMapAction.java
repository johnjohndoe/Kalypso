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
package org.kalypso.model.flood.ui.map;

import java.math.BigDecimal;

import org.apache.commons.lang3.Range;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.gml.ui.KalypsoGmlUIPlugin;
import org.kalypso.gml.ui.KalypsoGmlUiImages;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypso.model.flood.binding.ITinReference;
import org.kalypso.model.flood.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.graphics.sld.PolygonColorMap;

/**
 * @author Gernot Belger
 */
class GenerateColorMapAction extends Action
{
  private final EventManagementWidget m_widget;

  public GenerateColorMapAction( final EventManagementWidget widget )
  {
    m_widget = widget;

    setText( Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.8" ) ); //$NON-NLS-1$
    setToolTipText( Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.9" ) ); //$NON-NLS-1$

    // We are reusing images of KalypsoGmlUi here
    final ImageDescriptor generateID = KalypsoGmlUIPlugin.getImageProvider().getImageDescriptor( KalypsoGmlUiImages.DESCRIPTORS.STYLE_EDIT );
    setImageDescriptor( generateID );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.display.getActiveShell();

    final IRunoffEvent runoffEvent = m_widget.getSelectedEvent();
    if( runoffEvent == null )
    {
      MessageDialog.openInformation( shell, getText(), Messages.getString("GenerateColorMapAction.0") ); //$NON-NLS-1$
      return;
    }

    final IKalypsoFeatureTheme runoffEventTheme = m_widget.findThemeForEvent( runoffEvent );

    final PolygonColorMap colorMap = m_widget.findColorMap( runoffEventTheme );
    if( colorMap == null )
      return;

    final IFeatureBindingCollection<ITinReference> tins = runoffEvent.getTins();
    if( tins.size() == 0 && colorMap.getColorMap().length == 0 )
    {
      MessageDialog.openInformation( shell, getText(), Messages.getString("GenerateColorMapAction.1") ); //$NON-NLS-1$
      return;
    }

    final Range<BigDecimal> range = computeTinRange( tins.toArray( new ITinReference[tins.size()] ) );
    final BigDecimal minimum = range.getMinimum();
    final BigDecimal maximum = range.getMaximum();

    final EventStyleDialog dialog = new EventStyleDialog( shell, colorMap, decimalOrNull( minimum ), decimalOrNull( maximum ) );
    if( dialog.open() == Window.OK )
      m_widget.handleColormapChanged( runoffEventTheme );
  }

  private BigDecimal decimalOrNull( final BigDecimal minimum )
  {
    if( Math.abs( minimum.doubleValue() ) > Double.MAX_VALUE - 1 )
      return null;

    return minimum;
  }

  static Range<BigDecimal> computeTinRange( final ITinReference[] tins )
  {
    // get min / max of the selected runoff event

    // REMARK: use min/max == Double.MAX_VALUE because the range does not support null values
    BigDecimal event_min = new BigDecimal( Double.MAX_VALUE );
    BigDecimal event_max = new BigDecimal( -Double.MAX_VALUE );

    for( final ITinReference tin : tins )
    {
      final BigDecimal min = tin.getMin();
      if( min != null && min.compareTo( event_min ) == -1 )
      {
        event_min = min;
      }

      final BigDecimal max = tin.getMax();
      if( max != null && max.compareTo( event_max ) == 1 )
      {
        event_max = max;
      }
    }

    return Range.between( event_min, event_max );
  }
}