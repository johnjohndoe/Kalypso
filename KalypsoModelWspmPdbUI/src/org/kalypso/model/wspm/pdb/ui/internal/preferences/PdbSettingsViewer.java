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
package org.kalypso.model.wspm.pdb.ui.internal.preferences;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.kalypso.model.wspm.pdb.connect.IPdbSettings;
import org.kalypso.model.wspm.pdb.connect.PdbSettings;

/**
 * @author Gernot Belger
 */
public class PdbSettingsViewer
{
  private TableViewer m_viewer;

  public TableViewer createViewer( final Composite parent )
  {
    m_viewer = new TableViewer( parent, SWT.SINGLE | SWT.BORDER );
    final Table table = m_viewer.getTable();
    table.setHeaderVisible( false );

    m_viewer.setContentProvider( new ArrayContentProvider() );
    m_viewer.setLabelProvider( new SettingsLabelProvider( "%s - %s" ) ); //$NON-NLS-1$
    m_viewer.setSorter( new ViewerSorter() );

    reset();

    return m_viewer;
  }

  public List<IPdbSettings> getInput( )
  {
    return (List<IPdbSettings>) m_viewer.getInput();
  }

  public void reset( )
  {
    // Get connections and clone into list; we are going to change the list
    final IPdbSettings[] connections = PdbSettings.getSettingsOrError();
    final List<IPdbSettings> input = new ArrayList<>( Arrays.asList( connections ) );
    m_viewer.setInput( input );
  }
}