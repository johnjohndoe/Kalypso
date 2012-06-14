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
package org.kalypso.ui.rrm.internal.cm.view;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ColumnViewer;
import org.eclipse.jface.viewers.DialogCellEditor;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.model.hydrology.binding.timeseries.IStationCollection;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.model.hydrology.binding.timeseriesMappings.TimeseriesMappingType;
import org.kalypso.model.hydrology.timeseries.Timeserieses;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.rrm.internal.gml.feature.view.FindTimeseriesLinkRunnable;
import org.kalypso.ui.rrm.internal.gml.feature.view.dialogs.ChooseTimeseriesDialog;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class MappingElementBeanLinkEditingSupport extends EditingSupport
{
  private final TimeseriesMappingType m_type;

  public MappingElementBeanLinkEditingSupport( final ColumnViewer viewer, final TimeseriesMappingType type )
  {
    super( viewer );

    m_type = type;
  }

  @Override
  protected CellEditor getCellEditor( final Object element )
  {
    final String parameterType = m_type.getLinkParameterType();

    return new DialogCellEditor( (Composite) getViewer().getControl() )
    {
      @Override
      protected Object openDialogBox( final Control cellEditorWindow )
      {
        final IStationCollection collection = FindTimeseriesLinkRunnable.getStationCollection();
        final CommandableWorkspace stationsWorkspace = FindTimeseriesLinkRunnable.getStationsWorkspace();

        final ChooseTimeseriesDialog dialog = new ChooseTimeseriesDialog( cellEditorWindow.getShell(), stationsWorkspace, collection, parameterType );

        final MappingElementBean bean = (MappingElementBean) element;
        final ITimeseries oldValue = bean.getTimeseries();
        dialog.setSelection( oldValue );

        if( dialog.open() == Window.OK )
        {
          final ITimeseries newValue = dialog.getSelection();

          // REMARK: necessary, because null will not be applied, but we want to be able to delete the link...
          bean.setTimeseries( newValue );

          getViewer().update( bean, null );

          updateContents( newValue );

          return newValue;
        }
        else
          return oldValue;
      }
    };
  }

  @Override
  protected boolean canEdit( final Object element )
  {
    return true;
  }

  @Override
  protected Object getValue( final Object element )
  {
    final MappingElementBean bean = (MappingElementBean) element;

    final ITimeseries timeseries = bean.getTimeseries();
    if( timeseries == null )
      return Messages.getString("MappingElementBeanLinkEditingSupport_0"); //$NON-NLS-1$

    return Timeserieses.toLinkLabel( timeseries );
  }

  @Override
  protected void setValue( final Object element, final Object value )
  {
    // nothing to do, all was done in openDialogBox

// final MappingElementBean bean = (MappingElementBean) element;
// bean.setTimeseries( (ITimeseries) value );
//
// getViewer().update( bean, null );
  }
}