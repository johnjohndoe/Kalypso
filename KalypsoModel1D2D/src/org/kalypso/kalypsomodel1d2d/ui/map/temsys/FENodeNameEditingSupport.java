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

import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.kalypso.afgui.model.Util;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerUtilities;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeFeatureNameCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;

/**
 * @author Gernot Belger
 */
public class FENodeNameEditingSupport extends EditingSupport
{
  private final CellEditor m_cellEditor;

  public FENodeNameEditingSupport( final TableViewer viewer )
  {
    super( viewer );

    m_cellEditor = new TextCellEditor( (Composite) viewer.getControl() );
    ((Text) m_cellEditor.getControl()).setTextLimit( 15 );
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
    final IFE1D2DNode node = (IFE1D2DNode) element;
    final String name = node.getName();
    if( StringUtils.isBlank( name ) )
      return StringUtils.EMPTY;

    return name;
  }

  @Override
  protected void setValue( final Object element, final Object value )
  {
    final IFE1D2DNode node = (IFE1D2DNode) element;

    final Object oldValObject = getValue( element );
    if( oldValObject.equals( value ) )
      return;

    final CommandableWorkspace workspace = Util.getCommandableWorkspace( IFEDiscretisationModel1d2d.class );

    final ChangeFeatureNameCommand cmd = new ChangeFeatureNameCommand( node, (String) value )
    {
      @Override
      public void process( ) throws Exception
      {
        super.process();

        ViewerUtilities.update( getViewer(), new Object[] { node }, null, true );
      }
    };
    try
    {
      workspace.postCommand( cmd );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }
}