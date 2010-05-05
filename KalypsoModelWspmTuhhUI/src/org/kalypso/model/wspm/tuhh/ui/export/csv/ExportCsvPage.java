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
package org.kalypso.model.wspm.tuhh.ui.export.csv;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserDelegateSave;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResult;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultLengthSection;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultLengthSectionColumn;
import org.kalypso.model.wspm.tuhh.ui.export.ExportFileChooserPage;
import org.kalypso.observation.result.IComponent;

/**
 * @author Gernot Belger
 */
public class ExportCsvPage extends ExportFileChooserPage
{
  private static final String FILTER_LABEL = "Comma Separated File";

  private static final String EXTENSION = "csv";

  private final CsvExportResultChooser m_resultChooser;

  private CsvExportComponentChooser m_componentChooser;

  public ExportCsvPage( final IWspmResultNode results )
  {
    super( new FileChooserDelegateSave(), EXTENSION );

    ((FileChooserDelegateSave) getFileChooserDelegate()).addFilter( FILTER_LABEL, "*." + EXTENSION );

    m_resultChooser = new CsvExportResultChooser( results );
    m_resultChooser.addCheckStateListener( new ICheckStateListener()
    {
      @Override
      public void checkStateChanged( final CheckStateChangedEvent event )
      {
        updateMessage();
      }
    } );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.export.ExportFileChooserPage#createPageContent(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected void createPageContent( final Composite parent )
  {
    super.createPageContent( parent );

    final Composite resultGroup = createResultGroup( parent );
    resultGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
  }

  private Composite createResultGroup( final Composite parent )
  {
    m_componentChooser = new CsvExportComponentChooser( getDialogSettings() );
    m_componentChooser.addCheckStateListener( new ICheckStateListener()
    {
      @Override
      public void checkStateChanged( final CheckStateChangedEvent event )
      {
        updateMessage();
      }
    } );

    final Group group = new Group( parent, SWT.NONE );
    final GridLayout layout = new GridLayout( 1, false );
    group.setLayout( layout );
    group.setText( "Weitere Daten" );

    final SashForm sashForm = new SashForm( group, SWT.HORIZONTAL );
    sashForm.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    m_resultChooser.createControl( sashForm );
    m_componentChooser.createControl( sashForm );

    sashForm.setWeights( new int[] { 50, 50 } );

    return group;
  }

  public WspmResultLengthSectionColumn[] getSelectedColumns( )
  {
    final Collection<WspmResultLengthSectionColumn> columns = new ArrayList<WspmResultLengthSectionColumn>();

    final IWspmResultNode[] results = m_resultChooser.getSelectedResults();
    final IComponent[] components = m_componentChooser.getSelectedComponents();

    for( final IWspmResultNode result : results )
    {
      if( result instanceof IWspmResult )
      {
        final WspmResultLengthSection section = ((IWspmResult) result).getLengthSection();
        for( final IComponent component : components )
        {
          if( section.hasColumn( component ) )
          {
            final WspmResultLengthSectionColumn column = section.getColumn( component );
            columns.add( column );
          }
        }
      }
    }

    return columns.toArray( new WspmResultLengthSectionColumn[columns.size()] );
  }

}
