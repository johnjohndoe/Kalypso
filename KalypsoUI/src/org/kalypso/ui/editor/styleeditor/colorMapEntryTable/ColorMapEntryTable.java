/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.editor.styleeditor.colorMapEntryTable;

import java.awt.Color;
import java.util.Arrays;
import java.util.Iterator;
import java.util.TreeMap;
import java.util.Vector;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ColorCellEditor;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ui.ImageProvider;
import org.kalypsodeegree.graphics.sld.ColorMapEntry;
import org.kalypsodeegree.graphics.sld.RasterSymbolizer;
import org.kalypsodeegree_impl.graphics.sld.ColorMapEntry_Impl;

public class ColorMapEntryTable
{
  public static Table table = null;

  public TableViewer tableViewer = null;

  public ColorMapEntryList m_colorMapEntryList = null;

  KalypsoUserStyle m_userStyle = null;

  RasterSymbolizer m_rasterSymbolizer = null;

  // Set the table column property names
  private final String LABEL_COLUMN = "label";

  private final String QUANTITY_COLUMN = "quantity";

  private final String COLOR_COLUMN = "color";

  private final String OPACITY_COLUMN = "opacity";

  // Set column names
  private final String[] columnNames = new String[] { LABEL_COLUMN, QUANTITY_COLUMN, COLOR_COLUMN, OPACITY_COLUMN };

  private static final String fNON_NEGATIVE_INTEGER_FIELD = "(\\d){1,9}";

  private static final String fINTEGER_FIELD = "(-)?" + fNON_NEGATIVE_INTEGER_FIELD;

  private static final String fNON_NEGATIVE_FLOATING_POINT_FIELD = "(\\d){1,10}\\.(\\d){1,10}";

  private static final String fFLOATING_POINT_FIELD = "(-)?" + fNON_NEGATIVE_FLOATING_POINT_FIELD;

  /**
   * for testing
   */
  public ColorMapEntryTable( final Composite parent )
  {
    m_colorMapEntryList = new ColorMapEntryList();
    final int count = 10;
    ColorMapEntry colorMapEntry;
    for( int i = 0; i < count; i++ )
    {
      colorMapEntry = new ColorMapEntry_Impl( Color.CYAN, i * 0.1, i, "Label " + i );
      m_colorMapEntryList.addColorMapEntry( colorMapEntry );
    }
    this.addChildControls( parent );
  }

  public ColorMapEntryTable( final Composite parent, final KalypsoUserStyle userStyle, final RasterSymbolizer rasterSymbolizer )
  {
    m_userStyle = userStyle;
    m_rasterSymbolizer = rasterSymbolizer;
    m_colorMapEntryList = new ColorMapEntryList();
    final TreeMap colorMap = m_rasterSymbolizer.getColorMap();
    final Iterator it = colorMap.keySet().iterator();
    while( it.hasNext() )
    {
      final Double key = (Double) it.next();
      final ColorMapEntry colorMapEntry = (ColorMapEntry) colorMap.get( key );
      m_colorMapEntryList.addColorMapEntry( colorMapEntry.clone() );

    }

    this.addChildControls( parent );
  }

  private void addChildControls( final Composite composite )
  {
    // Create a composite to hold the children
    final GridData gridData = new GridData( GridData.HORIZONTAL_ALIGN_FILL | GridData.FILL_BOTH );
    composite.setLayoutData( gridData );

    // Set numColumns to 3 for the buttons
    final GridLayout layout = new GridLayout( 3, false );
    layout.marginWidth = 4;
    composite.setLayout( layout );

    // Create the table
    createTable( composite );

    // Create and setup the TableViewer
    createTableViewer();
    tableViewer.setContentProvider( new ColorMapEntryContentProvider() );
    tableViewer.setLabelProvider( new ColorMapEntryLabelProvider() );

    tableViewer.setInput( m_colorMapEntryList );

    // Add the buttons
    createButtons( composite );
  }

  /**
   * Create the Table
   */
  private void createTable( final Composite parent )
  {
    final int style = SWT.SINGLE | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION | SWT.HIDE_SELECTION;

    table = new Table( parent, style );

    final GridData gridData = new GridData( GridData.FILL_BOTH );
    gridData.grabExcessVerticalSpace = true;
    gridData.horizontalSpan = 3;
    table.setLayoutData( gridData );

    table.setLinesVisible( true );
    table.setHeaderVisible( true );

    // 1st column with label
    TableColumn column = new TableColumn( table, SWT.CENTER, 0 );
    column.setText( columnNames[0] );
    column.setWidth( 70 );

    // 2nd column with quantity
    column = new TableColumn( table, SWT.CENTER, 1 );
    column.setText( columnNames[1] );
    column.setWidth( 60 );
    // Add listener to column so tasks are sorted by quantity when clicked
    column.addSelectionListener( new SelectionAdapter()
    {

      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        tableViewer.setSorter( new QuantitySorter() );
      }
    } );

    // 3rd column with color
    column = new TableColumn( table, SWT.CENTER, 2 );
    column.setText( columnNames[2] );
    column.setWidth( 50 );

    // 4th column with opacity
    column = new TableColumn( table, SWT.CENTER, 3 );
    column.setText( columnNames[3] );
    column.setWidth( 50 );
  }

  /**
   * Create the TableViewer
   */
  private void createTableViewer( )
  {

    tableViewer = new TableViewer( table );
    tableViewer.setUseHashlookup( true );

    tableViewer.setColumnProperties( columnNames );

    // Create the cell editors
    final CellEditor[] editors = new CellEditor[columnNames.length];

    // Column 1 :
    TextCellEditor textEditor = new TextCellEditor( table );
    ((Text) textEditor.getControl()).setTextLimit( 100 );
    editors[0] = textEditor;

    // Column 2 :
    textEditor = new TextCellEditor( table );

    ((Text) textEditor.getControl()).addVerifyListener(

    new VerifyListener()
    {
      public void verifyText( final VerifyEvent e )
      {
        if( e.text.matches( fINTEGER_FIELD ) || e.text.matches( fFLOATING_POINT_FIELD ) )
          e.doit = true;
      }
    } );

    editors[1] = textEditor;

    // Column 3 :
    editors[2] = new ColorCellEditor( table );

    // Column 4 :
    textEditor = new TextCellEditor( table );
    ((Text) textEditor.getControl()).addVerifyListener(

    new VerifyListener()
    {
      public void verifyText( final VerifyEvent e )
      {
        if( e.text.matches( fNON_NEGATIVE_INTEGER_FIELD ) || e.text.matches( fNON_NEGATIVE_FLOATING_POINT_FIELD ) )
          e.doit = true;
      }
    } );
    editors[3] = textEditor;

    // Assign the cell editors to the viewer
    tableViewer.setCellEditors( editors );
    // Set the cell modifier for the viewer
    tableViewer.setCellModifier( new ColorMapEntryCellModifier( this ) );
    // Set the default sorter for the viewer
    tableViewer.setSorter( new QuantitySorter() );
  }

  class ColorMapEntryContentProvider implements IStructuredContentProvider, IColorMapEntryViewer
  {
    public void inputChanged( final Viewer v, final Object oldInput, final Object newInput )
    {
      if( newInput != null )
        ((ColorMapEntryList) newInput).addChangeListener( this );
      if( oldInput != null )
        ((ColorMapEntryList) oldInput).removeChangeListener( this );
    }

    public void dispose( )
    {
      m_colorMapEntryList.removeChangeListener( this );
    }

    // Return the colorMapEntries as an array of Objects
    public Object[] getElements( final Object parent )
    {
      return m_colorMapEntryList.getColorMapEntries().toArray();
    }

    public void addColorMapEntry( final ColorMapEntry colorMapEntry )
    {
      tableViewer.add( colorMapEntry );
    }

    public void removeColorMapEntry( final ColorMapEntry colorMapEntry )
    {
      tableViewer.remove( colorMapEntry );
    }

    public void updateColorMapEntry( final ColorMapEntry colorMapEntry )
    {
      tableViewer.update( colorMapEntry, null );
    }
  }

  /**
   * Add the "Add", "Delete" and "Analyse","Refresh" buttons
   * 
   * @param parent
   *            the parent composite
   */
  private void createButtons( final Composite parent )
  {

    // Create and configure the "Add" button
    final Button add = new Button( parent, SWT.PUSH | SWT.CENTER );
    add.setImage( ImageProvider.IMAGE_STYLEEDITOR_ADD_RULE.createImage() );

    GridData gridData = new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING );
    gridData.widthHint = 30;
    add.setLayoutData( gridData );
    add.addSelectionListener( new SelectionAdapter()
    {

      // Add a colorMapEntry to the colorMapEntryMap and refresh the table
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final ColorMapEntry colorMapEntry = new ColorMapEntry_Impl( Color.WHITE, 1, 0, "" );
        m_colorMapEntryList.addColorMapEntry( colorMapEntry );
      }
    } );

    // Create and configure the "Delete" button
    final Button delete = new Button( parent, SWT.PUSH | SWT.CENTER );
    delete.setImage( ImageProvider.IMAGE_STYLEEDITOR_REMOVE.createImage() );
    gridData = new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING );
    gridData.widthHint = 30;
    delete.setLayoutData( gridData );

    delete.addSelectionListener( new SelectionAdapter()
    {

      // Remove the selection and refresh the table
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final ColorMapEntry colorMapEntry = (ColorMapEntry) ((IStructuredSelection) tableViewer.getSelection()).getFirstElement();
        if( colorMapEntry != null )
        {
          m_colorMapEntryList.removeColorMapEntry( colorMapEntry );
        }
      }
    } );

    // Create and configure the "Refresh Map" button
    final Button refresh = new Button( parent, SWT.PUSH | SWT.CENTER );
    refresh.setText( "refresh" );
    gridData = new GridData( GridData.HORIZONTAL_ALIGN_END );
    gridData.widthHint = 70;
    refresh.setLayoutData( gridData );

    refresh.addSelectionListener( new SelectionAdapter()
    {

      // refresh the map
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        updateRasterSymbolizer();
      }
    } );

    // Create and configure the "Analyse" button
    /*
     * Button analyse = new Button( parent, SWT.PUSH | SWT.CENTER ); analyse.setText( "analyse" ); gridData = new
     * GridData( GridData.HORIZONTAL_ALIGN_END ); gridData.widthHint = 50; analyse.setLayoutData( gridData );
     * analyse.addSelectionListener( new SelectionAdapter() { // analyse the raster data public void widgetSelected(
     * SelectionEvent e ) { System.out.println("Analyse"); } } );
     */

  }

  /**
   * Return the column names in a collection
   * 
   * @return List containing column names
   */
  public java.util.List getColumnNames( )
  {
    return Arrays.asList( columnNames );
  }

  /**
   * @return currently selected item
   */
  public ISelection getSelection( )
  {
    return tableViewer.getSelection();
  }

  /**
   * Return the ColorMapEntryList
   */
  public ColorMapEntryList getColorMapEntryList( )
  {
    return m_colorMapEntryList;
  }

  /**
   * update the colorMap of the rasterSymbolizer
   */
  public void updateRasterSymbolizer( )
  {
    final TreeMap<Double, ColorMapEntry> new_colorMap = new TreeMap<Double, ColorMapEntry>();
    final Vector colorMapEntries = m_colorMapEntryList.getColorMapEntries();
    try
    {
      for( int i = 0; i < colorMapEntries.size(); i++ )
      {
        final ColorMapEntry colorMapEntry = (ColorMapEntry) colorMapEntries.get( i );
        if( !new_colorMap.containsKey( new Double( colorMapEntry.getQuantity() ) ) )
          new_colorMap.put( new Double( colorMapEntry.getQuantity() ), colorMapEntry.clone() );
        else
        {
          throw new Exception();
        }
      }
      m_rasterSymbolizer.setColorMap( new_colorMap );
      m_userStyle.fireStyleChanged();
    }
    catch( final Exception e )
    {
      MessageDialog.openError( table.getShell(), "Error", "Quantities must have unique values!" );
    }
  }

}