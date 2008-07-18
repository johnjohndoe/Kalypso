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
package org.kalypso.transformation.ui;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.deegree.model.crs.CoordinateSystem;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.kalypso.transformation.CRSHelper;
import org.kalypso.transformation.CachedCRSFactory;

/**
 * This class represents a panel with elements for managing the coordinate systems.
 * 
 * @author Holger Albert
 */
public class AvailableCRSPanel extends Composite
{
  /**
   * The list of available crs panel listener.
   */
  private List<IAvailableCRSPanelListener> m_listener;

  /**
   * The list viewer of the coordinate systems. Null, if no controls has been created.
   */
  protected ListViewer m_viewer;

  /**
   * A hash of the displayed coordinate systems.
   */
  protected HashMap<String, CoordinateSystem> m_coordHash;

  /**
   * The construtor.
   */
  public AvailableCRSPanel( Composite parent, int style )
  {
    super( parent, style );

    m_listener = new ArrayList<IAvailableCRSPanelListener>();
    m_viewer = null;
    m_coordHash = new HashMap<String, CoordinateSystem>();

    /* Create the controls. */
    createControls();
  }

  /**
   * This function creates the controls.
   */
  private void createControls( )
  {
    /* Set the layout data. */
    GridLayout gridLayout = new GridLayout( 1, false );
    gridLayout.horizontalSpacing = 0;
    gridLayout.verticalSpacing = 0;
    gridLayout.marginHeight = 0;
    gridLayout.marginWidth = 0;
    super.setLayout( gridLayout );

    /* Create the main group for the panel. */
    Group main = new Group( this, SWT.NONE );
    main.setLayout( new GridLayout( 3, false ) );
    main.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    main.setText( "Verfügbare Koordinaten-Systeme" );

    /* Create the combo. */
    m_viewer = new ListViewer( main, SWT.BORDER | SWT.READ_ONLY | SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL );
    m_viewer.getList().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false, 3, 0 ) );
    m_viewer.setContentProvider( new ArrayContentProvider() );
    m_viewer.setLabelProvider( new CRSLabelProvider( false ) );
    m_viewer.setSorter( new ViewerSorter() );

    /* Create the info image. */
    final Label imageLabel = new Label( main, SWT.NONE );
    imageLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, true, false ) );

    /* Set the image. */
    ImageDescriptor imgDesc = ImageDescriptor.createFromURL( getClass().getResource( "resources/info.gif" ) );
    Image infoImage = imgDesc.createImage();
    imageLabel.setImage( infoImage );

    m_viewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      /**
       * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
       */
      public void selectionChanged( SelectionChangedEvent event )
      {
        /* Get the name of the selected coordinate system. */
        String selectedCRS = getSelectedCRS();
        if( selectedCRS == null )
        {
          imageLabel.setToolTipText( "" );
          return;
        }

        /* Get the hashed coordinate system. */
        imageLabel.setToolTipText( CRSHelper.getTooltipText( selectedCRS ) );
      }
    } );

    /* Create the button. */
    Button removeButton = new Button( main, SWT.PUSH );
    removeButton.setLayoutData( new GridData( SWT.END, SWT.CENTER, false, false ) );
    removeButton.setText( "Entfernen" );

    /* Add a listener. */
    removeButton.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        handleRemovePressed();
      }
    } );

    /* Create the button. */
    Button addButton = new Button( main, SWT.PUSH );
    addButton.setLayoutData( new GridData( SWT.END, SWT.CENTER, false, false ) );
    addButton.setText( "Hinzufügen" );

    /* Add a listener. */
    addButton.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        handleAddPressed( e.display );
      }
    } );
  }

  /**
   * @see org.eclipse.swt.widgets.Composite#setLayout(org.eclipse.swt.widgets.Layout)
   */
  @Override
  public void setLayout( Layout layout )
  {
    /* Ignore user set layouts, only layout datas are permitted. */
  }

  /**
   * @see org.eclipse.swt.widgets.Control#setEnabled(boolean)
   */
  @Override
  public void setEnabled( boolean enabled )
  {
    super.setEnabled( enabled );

    m_viewer.getControl().setEnabled( enabled );
  }

  /**
   * This function disposes the images.
   */
  @Override
  public void dispose( )
  {
    super.dispose();
  }

  /**
   * This function sets the available coordinate systems.
   * 
   * @param preferenceNames
   *            An array of names from coordinate systems. Make sure, they are ; seperated.
   */
  public void setAvailableCoordinateSystems( String preferenceNames )
  {
    try
    {
      if( preferenceNames == null || preferenceNames.length() == 0 )
      {
        m_coordHash.clear();
        m_viewer.setInput( null );
        return;
      }

      /* The names of the coordinate systems as array. */
      String[] namesArray = preferenceNames.split( ";" );

      /* Get all coordinate system names. */
      List<String> names = Arrays.asList( namesArray );

      /* Create a hash of it. */
      m_coordHash = CRSHelper.getCoordHash( names );

      /* Get all coordinate systems. */
      List<CoordinateSystem> coordinateSystems = CRSHelper.getCRSListByNames( names );

      /* Set the input. */
      m_viewer.setInput( coordinateSystems );

      /* Notify it about the initializing. */
      fireCoordinateSystemsInitialized( names );
    }
    catch( Exception ex )
    {
      ex.printStackTrace();
    }
  }

  /**
   * This function returns the names of all coordinate systems in the list and returns them as a ; seperated string.
   * 
   * @return The names of the coordinate systems in the list as a ; seperated string.
   */
  public String getAvailableCoordinateSystems( )
  {
    /* Memory for the keys. */
    String preferenceNames = "";

    /* Get the iterator for the keys. */
    Iterator<String> iterator = m_coordHash.keySet().iterator();

    /* Iterate over the keys. */
    while( iterator.hasNext() )
    {
      /* Get the next key. */
      String key = iterator.next();

      /* Add it to the string. */
      preferenceNames = preferenceNames + key;

      /* Make sure, the names are ; seperated. */
      if( iterator.hasNext() )
        preferenceNames = preferenceNames + ";";
    }

    return preferenceNames;
  }

  /**
   * This function sets the selection of the panel.
   * 
   * @param selection
   *            The selection.
   */
  public void setSelectedCRS( String selectedCRS )
  {
    if( m_viewer != null )
    {
      CoordinateSystem coordinateSystem = m_coordHash.get( selectedCRS );
      if( coordinateSystem != null )
        m_viewer.setSelection( new StructuredSelection( coordinateSystem ) );
    }
  }

  /**
   * This function returns the name of the selected coordinate system.
   * 
   * @return The name of the selected coordinate system or null, if none is selected.
   */
  public String getSelectedCRS( )
  {
    /* Get the selection. */
    ISelection selection = m_viewer.getSelection();

    /* The name of the crs, which will be told to the listeners. */
    String selectedCRS = null;

    /* If not empty and the right type, the name is set. */
    if( !selection.isEmpty() && selection instanceof IStructuredSelection )
    {
      /* Cast. */
      IStructuredSelection structuredSelection = (IStructuredSelection) selection;

      /* Get the selected element. */
      Object selectedElement = structuredSelection.getFirstElement();

      /* Check type. */
      if( selectedElement instanceof CoordinateSystem )
      {
        /* Cast. */
        CoordinateSystem coordinateSystem = (CoordinateSystem) selectedElement;

        /* Set the name of the selected coordinate system. */
        selectedCRS = coordinateSystem.getIdentifier();
      }
    }

    return selectedCRS;
  }

  /**
   * This function adds a available crs panel listener.
   * 
   * @param listener
   *            The available crs panel listener.
   */
  public void addAvailableCRSPanelListener( IAvailableCRSPanelListener listener )
  {
    if( !m_listener.contains( listener ) )
      m_listener.add( listener );
  }

  /**
   * This function removes a available crs panel listener.
   * 
   * @param listener
   *            The available crs panel listener.
   */
  public void removeAvailableCRSPanelListener( IAvailableCRSPanelListener listener )
  {
    if( m_listener.contains( listener ) )
      m_listener.remove( listener );
  }

  /**
   * This function adds a selection changed listener.
   * 
   * @param listener
   *            The selection changed listener.
   */
  public void addSelectionChangedListener( ISelectionChangedListener listener )
  {
    if( m_viewer != null )
      m_viewer.addSelectionChangedListener( listener );
  }

  /**
   * This function removes a selection changed listener.
   * 
   * @param listener
   *            The selection changed listener.
   */
  public void removeSelectionChangedListener( ISelectionChangedListener listener )
  {
    if( m_viewer != null )
      m_viewer.removeSelectionChangedListener( listener );
  }

  /**
   * This function informs listeners about the coordinate system, which was removed.
   * 
   * @param names
   *            The list of names of the coordinate systems.
   */
  private void fireCoordinateSystemsInitialized( List<String> names )
  {
    for( int i = 0; i < m_listener.size(); i++ )
    {
      /* Get the available crs panel listener. */
      IAvailableCRSPanelListener availableCRSPanelListener = m_listener.get( i );

      /* Notify it about the initializing. */
      availableCRSPanelListener.coordinateSystemsInitialized( names );
    }
  }

  /**
   * This function removes the selected coordinate system.
   */
  protected void handleRemovePressed( )
  {
    /* Get the selected coordinate system. */
    String selectedCRS = getSelectedCRS();
    if( selectedCRS == null )
      return;

    /* Get the selected coordinate system. */
    CoordinateSystem coordinateSystem = m_coordHash.get( selectedCRS );
    if( coordinateSystem == null )
      return;

    /* Remove the selected coordinate system. */
    m_coordHash.remove( selectedCRS );

    /* Now remove it from the viewer, too. */
    m_viewer.remove( coordinateSystem );

    /* Reset the selection. */
    m_viewer.setSelection( new StructuredSelection() );

    /* Tell listeners, that a structure change has occured. */
    fireCoordinateSystemRemoved( coordinateSystem.getIdentifier() );
  }

  /**
   * This function informs listeners about the coordinate system, which was removed.
   * 
   * @param name
   *            The name of the coordinate system.
   */
  private void fireCoordinateSystemRemoved( String name )
  {
    for( int i = 0; i < m_listener.size(); i++ )
    {
      /* Get the available crs panel listener. */
      IAvailableCRSPanelListener availableCRSPanelListener = m_listener.get( i );

      /* Notify it about the removal. */
      availableCRSPanelListener.coordinateSystemRemoved( name );
    }
  }

  /**
   * This function adds a new coordinate system.
   * 
   * @param display
   *            The display.
   */
  protected void handleAddPressed( Display display )
  {
    try
    {
      /* Create the dialog for entering the EPSG code coordinate system. */
      InputDialog dialog = new InputDialog( display.getActiveShell(), "Koordinaten-System hinzufügen", "Bitte geben Sie den EPSG-Code eines Koordinaten-Systems ein:", "EPSG:", new CRSInputValidator() );
      int open = dialog.open();
      if( open == InputDialog.CANCEL )
        return;

      /* The entered coordinate system. */
      String name = dialog.getValue();

      /* Create it, the name should be already validated. */
      CoordinateSystem coordinateSystem = CachedCRSFactory.getInstance().create( name );

      /* Actualize the hash. */
      m_coordHash.put( coordinateSystem.getIdentifier(), coordinateSystem );

      /* Add it to the viewer. */
      m_viewer.add( coordinateSystem );

      /* Set the selection. */
      m_viewer.setSelection( new StructuredSelection( coordinateSystem ) );

      /* Tell listeners, that a structure change has occured. */
      fireCoordinateSystemAdded( coordinateSystem.getIdentifier() );
    }
    catch( Exception ex )
    {
      ex.printStackTrace();
    }
  }

  /**
   * This function informs listeners about the coordinate system, which was added.
   * 
   * @param name
   *            The name of the coordinate system.
   */
  private void fireCoordinateSystemAdded( String name )
  {
    for( int i = 0; i < m_listener.size(); i++ )
    {
      /* Get the available crs panel listener. */
      IAvailableCRSPanelListener availableCRSPanelListener = m_listener.get( i );

      /* Notify it about the addition. */
      availableCRSPanelListener.coordinateSystemAdded( name );
    }
  }
}