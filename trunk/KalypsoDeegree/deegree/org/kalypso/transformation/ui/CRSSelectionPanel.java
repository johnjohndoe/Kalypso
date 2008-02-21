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
package org.kalypso.transformation.ui;

import java.util.HashMap;
import java.util.List;

import org.deegree.model.crs.CoordinateSystem;
import org.deegree.model.crs.UnknownCRSException;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.kalypso.transformation.CRSHelper;

/**
 * This class represents a panel with elements for choosing a coordinate system.
 * 
 * @author Holger Albert
 */
public class CRSSelectionPanel
{
  /**
   * The combo viewer of the coordinate systems. Null, if no controls has been created.
   */
  protected ComboViewer m_viewer;

  /**
   * A hash of the displayed coordinate systems.
   */
  protected HashMap<String, CoordinateSystem> m_coordHash;

  /**
   * The constructor.
   */
  public CRSSelectionPanel( )
  {
    m_viewer = null;
    m_coordHash = null;
  }

  /**
   * This function creates the controls.
   * 
   * @param parent
   *            The parent.
   */
  public Control createControl( Composite parent )
  {
    /* Create the main group for the panel. */
    Group m_main = new Group( parent, SWT.NONE );
    m_main.setLayout( new GridLayout( 2, false ) );
    m_main.setText( "Koordinaten-System" );

    try
    {
      /* Create the combo. */
      m_viewer = new ComboViewer( m_main, SWT.NONE );
      m_viewer.getCombo().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
      m_viewer.setContentProvider( new ArrayContentProvider() );
      m_viewer.setLabelProvider( new CRSLabelProvider() );

      /* Get all coordinate system names. */
      List<String> names = CRSHelper.getAllNames();

      /* Get all coordinate systems. */
      List<CoordinateSystem> coordinateSystems = CRSHelper.getCRSListByNames( names );

      /* Cache the coordinate systems. */
      m_coordHash = new HashMap<String, CoordinateSystem>();
      for( int i = 0; i < coordinateSystems.size(); i++ )
      {
        CoordinateSystem coordinateSystem = coordinateSystems.get( i );
        m_coordHash.put( coordinateSystem.getIdentifier(), coordinateSystem );
      }

      /* Set the input. */
      m_viewer.setInput( coordinateSystems );

      /* Create the info image. */
      final Label imageLabel = new Label( m_main, SWT.NONE );
      imageLabel.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, false, false ) );

      /* Set the image. */
      ImageDescriptor imgDesc = ImageDescriptor.createFromURL( getClass().getResource( "resources/info.gif" ) );
      imageLabel.setImage( imgDesc.createImage() );

      /* Refresh the tooltip. */
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
          CoordinateSystem coordinateSystem = m_coordHash.get( selectedCRS );
          if( coordinateSystem == null )
          {
            imageLabel.setToolTipText( "" );
            return;
          }

          /* The tooltip. */
          String tooltip = "Identifier:\n";

          /* Get all identifiers. */
          String[] identifiers = coordinateSystem.getCRS().getIdentifiers();
          for( int i = 0; i < identifiers.length; i++ )
          {
            /* Get the identifier. */
            String identifier = identifiers[i];

            tooltip = tooltip + identifier+"\n";
          }

          imageLabel.setToolTipText( tooltip );
        }
      } );
    }
    catch( UnknownCRSException e )
    {
      e.printStackTrace();
    }

    return m_main;
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
      m_viewer.setSelection( new StructuredSelection( m_coordHash.get( selectedCRS ) ) );
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
        selectedCRS = coordinateSystem.getCRS().getIdentifier();
      }
    }

    return selectedCRS;
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
}