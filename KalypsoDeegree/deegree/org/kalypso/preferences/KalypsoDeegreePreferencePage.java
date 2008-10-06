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
package org.kalypso.preferences;

import java.util.Arrays;
import java.util.List;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.kalypso.transformation.ui.AvailableCRSPanel;
import org.kalypso.transformation.ui.CRSSelectionPanel;
import org.kalypso.transformation.ui.IAvailableCRSPanelListener;
import org.kalypsodeegree.KalypsoDeegreePlugin;

/**
 * The preference page for deegree things, like the coordinate system.
 * 
 * @author Holger Albert
 */
public class KalypsoDeegreePreferencePage extends PreferencePage implements IWorkbenchPreferencePage
{
  /**
   * This variable stores the GUI element for the coordinate system.
   */
  protected CRSSelectionPanel m_crsPanel;

  /**
   * This variable stores the coordinate system.
   */
  protected String m_coordinateSystem;

  /**
   * This variable stores the GUI element for the available coordinate systems.
   */
  protected AvailableCRSPanel m_availableCRSPanel;

  /**
   * This variable stores the available coordinate systems.
   */
  protected String m_availableCoordinateSystems;

  /**
   * The constructor.
   */
  public KalypsoDeegreePreferencePage( )
  {
    /* Initialize everything. */
    init();
  }

  public KalypsoDeegreePreferencePage( String title )
  {
    super( title );

    /* Initialize everything. */
    init();
  }

  public KalypsoDeegreePreferencePage( String title, ImageDescriptor image )
  {
    super( title, image );

    /* Initialize everything. */
    init();
  }

  /**
   * This function initializes everything.
   */
  private void init( )
  {
    m_crsPanel = null;
    m_coordinateSystem = null;

    m_availableCRSPanel = null;
    m_availableCoordinateSystems = null;

    /* Set the preference store. */
    setPreferenceStore( KalypsoDeegreePlugin.getDefault().getPreferenceStore() );

    /* Set the description. */
    setDescription( "Hier können Sie die Koordinaten-Systeme für Kalypso verwalten." );
  }

  /**
   * @see org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createContents( Composite parent )
  {
    /* Get the preference store. */
    IPreferenceStore store = getPreferenceStore();

    /* Create the main container. */
    Composite main = new Composite( parent, SWT.NONE );
    main.setLayout( new GridLayout( 1, false ) );

    /* Create the panel for the coordinate system. */
    m_crsPanel = new CRSSelectionPanel( main, SWT.NONE, "( Neustart erforderlich )" );
    m_crsPanel.setToolTipText( "Wählen Sie das Koordinaten-System aus, das Sie verwenden möchten (Neustart erforderlich)." );
    m_crsPanel.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    /* Add a listener. */
    m_crsPanel.addSelectionChangedListener( new ISelectionChangedListener()
    {
      /**
       * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
       */
      public void selectionChanged( SelectionChangedEvent event )
      {
        /* Store the coordinate system. */
        m_coordinateSystem = m_crsPanel.getSelectedCRS();

        /* Validate the page. */
        validatePage();
      }
    } );

    /* Set the old value. */
    m_crsPanel.setSelectedCRS( store.getString( IKalypsoDeegreePreferences.DEFAULT_CRS_SETTING ) );

    /* Create the panel for the available coordinate systems. */
    m_availableCRSPanel = new AvailableCRSPanel( main, SWT.NONE );
    m_availableCRSPanel.setToolTipText( "Management der verfügbaren Koordinaten-Systeme." );
    m_availableCRSPanel.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    /* Add a listener. */
    m_availableCRSPanel.addAvailableCRSPanelListener( new IAvailableCRSPanelListener()
    {
      /**
       * @see org.kalypso.transformation.ui.IAvailableCRSPanelListener#coordinateSystemsInitialized(java.util.List)
       */
      public void coordinateSystemsInitialized( List<String> names )
      {
        /* Store the available coordinate systems. */
        storeCoordinateSystems();
      }

      /**
       * @see org.kalypso.transformation.ui.IAvailableCRSPanelListener#coordinateSystemAdded(java.lang.String)
       */
      public void coordinateSystemAdded( String name )
      {
        /* Store the available coordinate systems. */
        storeCoordinateSystems();
      }

      /**
       * @see org.kalypso.transformation.ui.IAvailableCRSPanelListener#coordinateSystemRemoved(java.lang.String)
       */
      public void coordinateSystemRemoved( String name )
      {
        /* Store the available coordinate systems. */
        storeCoordinateSystems();
      }

      /**
       * This function stores the available coordinate systems.
       */
      private void storeCoordinateSystems( )
      {
        /* Store the available coordinate systems. */
        m_availableCoordinateSystems = m_availableCRSPanel.getAvailableCoordinateSystems();

        List<String> names = null;
        if( m_availableCoordinateSystems != null && m_availableCoordinateSystems.length() > 0 )
        {
          /* The names of the coordinate systems as array. */
          String[] namesArray = m_availableCoordinateSystems.split( ";" );

          /* Get all coordinate system names. */
          names = Arrays.asList( namesArray );
        }

        /* Update the combo box of the crs selection panel. */
        m_crsPanel.updateCoordinateSystemsCombo( names );

        /* Validate the page. */
        validatePage();
      }
    } );

    /* Set the old value. */
    m_availableCRSPanel.setAvailableCoordinateSystems( store.getString( IKalypsoDeegreePreferences.AVAILABLE_CRS_SETTING ) );

    /* Validate the page. */
    validatePage();

    return main;
  }

  /**
   * This function validates the page.
   */
  protected void validatePage( )
  {
    /* Reset the error message. */
    setErrorMessage( null );

    if( m_availableCoordinateSystems == null || m_availableCoordinateSystems.length() == 0 )
    {
      setErrorMessage( "Bitte wählen Sie mindestens ein verfügbares Koordinaten-System aus ..." );
      return;
    }

    if( m_coordinateSystem == null || m_coordinateSystem.length() == 0 )
    {
      setErrorMessage( "Bitte wählen Sie ein Koordinaten-System aus ..." );
      return;
    }
  }

  /**
   * @see org.eclipse.jface.preference.PreferencePage#performOk()
   */
  @Override
  public boolean performOk( )
  {
    performApply();
    return super.performOk();
  }

  /**
   * @see org.eclipse.jface.preference.PreferencePage#performApply()
   */
  @Override
  protected void performApply( )
  {
    /* Get the preference store. */
    IPreferenceStore store = getPreferenceStore();

    /* Set the new value. */
    store.setValue( IKalypsoDeegreePreferences.DEFAULT_CRS_SETTING, m_coordinateSystem );
    store.setValue( IKalypsoDeegreePreferences.AVAILABLE_CRS_SETTING, m_availableCoordinateSystems );

    /* Save the plugin preferences. */
    KalypsoDeegreePlugin.getDefault().savePluginPreferences();
  }

  /**
   * @see org.eclipse.jface.preference.PreferencePage#performDefaults()
   */
  @Override
  protected void performDefaults( )
  {
    /* Get the preference store. */
    IPreferenceStore store = getPreferenceStore();

    /* Must be set before, because the coordinate system in the combo will be checked against this ones. */
    if( m_availableCRSPanel != null && !m_availableCRSPanel.isDisposed() )
      m_availableCRSPanel.setAvailableCoordinateSystems( store.getDefaultString( IKalypsoDeegreePreferences.AVAILABLE_CRS_SETTING ) );

    if( m_crsPanel != null && !m_crsPanel.isDisposed() )
      m_crsPanel.setSelectedCRS( store.getDefaultString( IKalypsoDeegreePreferences.DEFAULT_CRS_SETTING ) );

  }

  /**
   * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
   */
  public void init( IWorkbench workbench )
  {
  }
}