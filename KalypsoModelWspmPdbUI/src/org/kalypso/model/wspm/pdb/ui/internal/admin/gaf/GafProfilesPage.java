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
package org.kalypso.model.wspm.pdb.ui.internal.admin.gaf;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.eclipse.core.databinding.beans.PojoProperties;
import org.eclipse.core.databinding.observable.list.WritableList;
import org.eclipse.core.databinding.property.value.IValueProperty;
import org.eclipse.jface.databinding.viewers.ViewerSupport;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.model.wspm.pdb.gaf.GafProfile;
import org.kalypso.model.wspm.pdb.gaf.GafProfiles;
import org.kalypso.model.wspm.pdb.gaf.ImportGafData;

/**
 * @author Gernot Belger
 */
public class GafProfilesPage extends WizardPage
{
  private final WritableList m_profiles = new WritableList( new ArrayList<GafProfile>(), GafProfile.class );

  private final ImportGafData m_data;

  private TableViewer m_profileViewer;

  private Text m_logView;

  protected GafProfilesPage( final String pageName, final ImportGafData data )
  {
    super( pageName );

    m_data = data;

    setTitle( "GAF Inhalt" );
    setDescription( "The following cross section have been read from the gaf file." );
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    setControl( panel );
    GridLayoutFactory.swtDefaults().numColumns( 2 ).equalWidth( true ).applyTo( panel );

    createProfileTable( panel ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    createLogView( panel ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
  }

  private Control createProfileTable( final Composite parent )
  {
    m_profileViewer = new TableViewer( parent, SWT.BORDER | SWT.FULL_SELECTION );

    final Table table = m_profileViewer.getTable();
    table.setHeaderVisible( true );

    table.addControlListener( new ColumnsResizeControlListener() );

    final TableViewerColumn nullColumn = new TableViewerColumn( m_profileViewer, SWT.BORDER );
    nullColumn.getColumn().setResizable( false );
    nullColumn.getColumn().setMoveable( false );
    nullColumn.getColumn().setData( ColumnsResizeControlListener.DATA_FIXED_COL_WIDTH, 0 );

    final TableViewerColumn stationColumn = new TableViewerColumn( m_profileViewer, SWT.BORDER );
    stationColumn.getColumn().setText( "Station" );
    stationColumn.getColumn().setResizable( false );
    stationColumn.getColumn().setData( ColumnsResizeControlListener.DATA_MIN_COL_WIDTH, ColumnsResizeControlListener.MIN_COL_WIDTH_PACK );
    stationColumn.getColumn().setAlignment( SWT.RIGHT );

    ColumnViewerSorter.registerSorter( stationColumn, new StationViewerSorter() );

    final IValueProperty stationProperty = PojoProperties.value( GafProfile.class, "station" ); //$NON-NLS-1$
    final IValueProperty nullProperty = PojoProperties.value( StringUtils.EMPTY );

    final IValueProperty[] labelProperties = new IValueProperty[] { nullProperty, stationProperty };
    ViewerSupport.bind( m_profileViewer, m_profiles, labelProperties );

    return table;
  }

  private Control createLogView( final Composite parent )
  {
    m_logView = new Text( parent, SWT.MULTI | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );

    m_logView.setEditable( false );

    final Font textFont = JFaceResources.getFont( JFaceResources.TEXT_FONT );
    m_logView.setFont( textFont );

    return m_logView;
  }

  public void updateControl( )
  {
    try
    {
      m_profiles.clear();
      final GafProfiles profiles = m_data.getProfiles();
      if( profiles != null )
        m_profiles.addAll( Arrays.asList( profiles.getProfiles() ) );

      final File logFile = m_data.getLogFile();
      final String logContent = FileUtils.readFileToString( logFile, Charset.defaultCharset().name() );
      m_logView.setText( logContent );
    }
    catch( final IOException e )
    {
      final StringWriter sw = new StringWriter();
      final PrintWriter pw = new PrintWriter( sw );
      e.printStackTrace( pw );
      pw.close();
      m_logView.setText( sw.toString() );
    }
  }
}
