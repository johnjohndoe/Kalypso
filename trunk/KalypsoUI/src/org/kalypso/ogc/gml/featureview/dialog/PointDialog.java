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
package org.kalypso.ogc.gml.featureview.dialog;

import java.rmi.RemoteException;
import java.util.Locale;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.swt.events.DoubleModifyListener;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypsodeegree_impl.model.cs.Adapters;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.kalypsodeegree_impl.model.cs.CoordinateSystem;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * This class builds the dialog for the data input for the GM_Point.
 * 
 * @author albert
 */
public class PointDialog extends Dialog
{
  private double m_values[];

  private CS_CoordinateSystem m_cs;

  private Label m_label[];

  private Text m_text[];

  public PointDialog( Shell parent, double[] values, CS_CoordinateSystem cs )
  {
    super( parent );

    m_values = values;
    m_cs = cs;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createDialogArea( final Composite parent )
  {
    /* Create a own composite for placing controls. */
    final Composite panel = (Composite) super.createDialogArea( parent );

    /* Configuring the composite. */
    final Shell shell = panel.getShell();
    shell.setText( "Point-Daten" );

    /* The label for the input data. */
    Label label = new Label( panel, SWT.NONE );
    label.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    label.setText( "Geben Sie hier die Werte und das Koordinaten-System an." );
    label.setAlignment( SWT.LEFT );

    /* A new group for the labels and texts. */
    final Group group = new Group( panel, SWT.NONE );
    group.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* Configuring the group. */
    group.setLayout( new GridLayout( 2, true ) );

    final Display display = shell.getDisplay();
    final Color badColor = display.getSystemColor( SWT.COLOR_RED );
    final Color goodColor = display.getSystemColor( SWT.COLOR_BLACK );

    final DoubleModifyListener doubleModifyListener = new DoubleModifyListener( goodColor, badColor );

    m_label = new Label[m_values.length];
    m_text = new Text[m_values.length];

    for( int i = 0; i < m_values.length; i++ )
    {
      /* The label for the input data. */
      m_label[i] = new Label( group, SWT.NONE );
      m_label[i].setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

      try
      {
        if( i < getCS_CoordinateSystem().getDimension() )
          m_label[i].setText( "Achse " + getCS_CoordinateSystem().getAxis( i ).name + " [" + getCS_CoordinateSystem().getUnits( i ).getName() + "]" );
        else
          m_label[i].setText( "nicht unterst¸tzt" );

      }
      catch( RemoteException e )
      {
        e.printStackTrace();
      }

      m_label[i].setAlignment( SWT.LEFT );

      /* The text for the input data. */
      m_text[i] = new Text( group, SWT.BORDER );
      m_text[i].setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
      m_text[i].setText( Double.toString( m_values[i] ) );

      m_text[i].addModifyListener( new PointDialogListener( this, i ) );
      m_text[i].addModifyListener( doubleModifyListener );
    }

    /* The label for the input data. */
    Label label1 = new Label( panel, SWT.NONE );
    label1.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    label1.setText( "Geben Sie hier das Koordinaten-System an." );
    label1.setAlignment( SWT.LEFT );
    
    /* A new group for the labels and texts. */
    final Group combo_group = new Group( panel, SWT.NONE );
    combo_group.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* Configuring the group. */
    combo_group.setLayout( new GridLayout( 2, true ) );
    
    /* The label for the coordinate system. */
    Label label2 = new Label( combo_group, SWT.NONE );
    label2.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    label2.setText( "Koordinaten-System: " );
    label2.setAlignment( SWT.LEFT );

    final ComboViewer combo = new ComboViewer( combo_group );
    ArrayContentProvider contentProvider = new ArrayContentProvider();
    LabelProvider labelProvider = new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( Object element )
      {
        CoordinateSystem crs = (CoordinateSystem) element;

        String name = "";

        name = crs.getName( Locale.getDefault() );

        return name;
      }
    };

    ConvenienceCSFactoryFull factory = new ConvenienceCSFactoryFull();
    CoordinateSystem[] crs = factory.getKnownCoordinateSystems();

    combo.setContentProvider( contentProvider );
    combo.setInput( crs );
    combo.setLabelProvider( labelProvider );

    try
    {
      combo.setSelection( new StructuredSelection( factory.getCSByName( getCS_CoordinateSystem().getName() ) ) );
    }
    catch( RemoteException e )
    {
      e.printStackTrace();
    }

    combo.addSelectionChangedListener( new ISelectionChangedListener()
    {

      public void selectionChanged( SelectionChangedEvent event )
      {
        final Adapters m_csAdapter = org.kalypsodeegree_impl.model.cs.Adapters.getDefault();
        CS_CoordinateSystem cs_crs = m_csAdapter.export( (CoordinateSystem) combo.getElementAt( combo.getCombo().getSelectionIndex() ) );
        setCS( cs_crs );
      }

    } );

    return panel;
  }

  /* Diese Funktion setzt den Status des OK-Buttons, nach dem Status der Text-Felder. */
  public void checkModified( )
  {
    this.getButton( IDialogConstants.OK_ID ).setEnabled( true );

    for( int i = 0; i < m_text.length; i++ )
    {
      if( !NumberUtils.isDouble( m_text[i].getText() ) )
      {
        this.getButton( IDialogConstants.OK_ID ).setEnabled( false );
      }
    }
  }

  public double[] getValues( )
  {
    return m_values;
  }

  public void setValues( double[] values )
  {
    m_values = values;
  }

  public CS_CoordinateSystem getCS_CoordinateSystem( )
  {
    return m_cs;
  }

  public void setCS( CS_CoordinateSystem cs )
  {
    m_cs = cs;
  }
}