/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;

/**
 * @author Gernot Belger
 */
public class CsvExportColumnsPage extends WizardPage
{
  enum OUTPUT_TYPE
  {
    point("einzelne Profilpunkte"),
    profiles("nur Profile");

    private final String m_label;

    private OUTPUT_TYPE( final String label )
    {
      m_label = label;
    }

    /**
     * @see java.lang.Enum#toString()
     */
    @Override
    public String toString( )
    {
      return m_label;
    }
  }

  private OUTPUT_TYPE m_type = OUTPUT_TYPE.point;

  protected CsvExportColumnsPage( )
  {
    super( "csvColumns" ); //$NON-NLS-1$

    setTitle( "Export Optionen" );
    setDescription( "Bitte wählen Sie auf dieser Seite die Optionen für den Export aus." );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout( 2, false ) );
    setControl( panel );

    createTypeControl( panel );
  }

  private Control createTypeControl( final Composite panel )
  {
    new Label( panel, SWT.NONE ).setText( "Ausgabeart" );

    final ComboViewer typeCombo = new ComboViewer( panel, SWT.READ_ONLY | SWT.DROP_DOWN );
    typeCombo.setContentProvider( new ArrayContentProvider() );
    typeCombo.setLabelProvider( new LabelProvider() );
    typeCombo.setInput( new OUTPUT_TYPE[] { OUTPUT_TYPE.point, OUTPUT_TYPE.profiles } );
    typeCombo.setSelection( new StructuredSelection( m_type ) );
    typeCombo.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        handleTypeChanged( (OUTPUT_TYPE) selection.getFirstElement() );
      }
    } );

    typeCombo.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    return typeCombo.getControl();
  }

  protected void handleTypeChanged( final OUTPUT_TYPE type )
  {
    m_type = type;
  }

  public OUTPUT_TYPE getType( )
  {
    return m_type;
  }
}
