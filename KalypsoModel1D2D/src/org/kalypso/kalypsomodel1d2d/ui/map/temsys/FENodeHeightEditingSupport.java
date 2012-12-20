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
package org.kalypso.kalypsomodel1d2d.ui.map.temsys;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ColumnViewer;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.ICellEditorValidator;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.afgui.model.Util;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerUtilities;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeNodeElevationCommand;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;

/**
 * @author Gernot Belger
 */
public class FENodeHeightEditingSupport extends EditingSupport
{
  private final ICellEditorValidator m_doubleValidator = new ICellEditorValidator()
  {
    @Override
    public String isValid( final Object value )
    {
      try
      {
        final String text = (String) value;
        if( StringUtils.isBlank( text ) )
          return null;

        NumberUtils.parseDouble( text );
        return null;
      }
      catch( final Throwable th )
      {
        return Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.AssignNodeElevationFaceComponent.11" ); //$NON-NLS-1$
      }
    }
  };

  private final CellEditor m_cellEditor;

  private final ApplyElevationWidgetDataModel m_dataModel;

  public FENodeHeightEditingSupport( final ColumnViewer viewer, final ApplyElevationWidgetDataModel dataModel )
  {
    super( viewer );

    m_dataModel = dataModel;

    m_cellEditor = new TextCellEditor( (Composite) viewer.getControl() );
    m_cellEditor.setValidator( m_doubleValidator );
  }

  @Override
  protected CellEditor getCellEditor( final Object element )
  {
    return m_cellEditor;
  }

  @Override
  protected boolean canEdit( final Object element )
  {
    return true;
  }

  @Override
  protected Object getValue( final Object element )
  {
    final double elevation = FENodeHeightProvider.getElevation( element );
    if( Double.isNaN( elevation ) )
      return StringUtils.EMPTY;

    return FENodeHeightProvider.getElevationString( element );
  }

  @Override
  protected void setValue( final Object element, final Object value )
  {
    if( !(element instanceof IFE1D2DNode) )
      return;

    final double elevation = NumberUtils.parseQuietDouble( ObjectUtils.toString( value ) );

    final IFE1D2DNode node = (IFE1D2DNode) element;

    final IFEDiscretisationModel1d2d model1d2d = m_dataModel.getDiscretisationModel();
    if( model1d2d == null )
      return;

    final CommandableWorkspace workspace = Util.getCommandableWorkspace( IFEDiscretisationModel1d2d.class );

    final ChangeNodeElevationCommand command = new ChangeNodeElevationCommand( model1d2d )
    {
      @Override
      public void process( )
      {
        super.process();

        refreshTable( node );
      }
    };

    command.addNodeElevation( node, elevation );

    try
    {
      workspace.postCommand( command );
    }
    catch( final Exception e )
    {
      // FIXME:error handling?
      e.printStackTrace();
    }
  }

  protected void refreshTable( final IFE1D2DNode node )
  {
    final IMapPanel mapPanel = m_dataModel.getMapPanel();
    mapPanel.invalidateMap();

    ViewerUtilities.update( getViewer(), new Object[] { node }, null, true );
  }
}