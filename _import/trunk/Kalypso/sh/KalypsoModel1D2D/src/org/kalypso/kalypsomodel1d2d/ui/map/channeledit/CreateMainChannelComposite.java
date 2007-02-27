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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.resource.ColorRegistry;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.ui.forms.widgets.Section;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.ui.view.ProfilViewData;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;
import org.kalypso.model.wspm.ui.view.chart.action.ProfilChartActionsEnum;
import org.kalypso.model.wspm.ui.view.chart.color.DefaultProfilColorRegistryFactory;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.widgets.SelectionWidget;
import org.kalypso.ogc.gml.map.widgets.mapfunctions.IRectangleMapFunction;
import org.kalypso.ogc.gml.widgets.IWidget;

/**
 * @author Thomas Jung
 */
public class CreateMainChannelComposite extends Composite
{
  private final ProfilViewData m_viewData = new ProfilViewData();

  private final ColorRegistry m_colorRegistry = DefaultProfilColorRegistryFactory.createColorRegistry( getDisplay() );

  private final CreateChannelData m_data;

  private final CreateMainChannelWidget m_widget;

  private Section m_profilSection;

  private Section m_segmentSection;

  /*********************************************************************************************************************
   * m_buttonList Following buttons are present in the Schlauchgenerator: Profile wählen Uferlinie 1 wählen Uferlinie 1
   * zeichnen Uferlinie 2 wählen Uferlinie 2 zeichnen
   */
  private final List<Button> m_buttonList = new ArrayList<Button>();

  private Button m_buttonConvertToModel;

  public CreateMainChannelComposite( final Composite parent, final int style, final CreateChannelData data, final CreateMainChannelWidget widget )
  {
    super( parent, style );

    m_data = data;
    m_widget = widget;

    try
    {
      init();
    }
    catch( Throwable e )
    {
      e.printStackTrace();
    }
  }

  /**
   * initialisation
   */
  private void init( )
  {
    /* Retrieve data */
    final IKalypsoFeatureTheme[] profileThemes = m_data.getProfileThemes();
    final IKalypsoFeatureTheme[] bankThemes = m_data.getBankThemes();

    /* Create gui */
    this.setLayout( new GridLayout( 1, false ) );

    /* Create Profile control */
    final Control profileSection = createProfileSelectionSection( this, profileThemes );
    GridData gridDataProf = new GridData( SWT.FILL, SWT.CENTER, true, false );
    profileSection.setLayoutData( gridDataProf );

    /* Create Bank control */
    final Control bankSection = createBankSelectionSection( this, bankThemes );
    GridData gridDataBank = new GridData( SWT.FILL, SWT.CENTER, true, false );
    bankSection.setLayoutData( gridDataBank );

    /* Create segment switch control */
    final Control segmentSwitchSection = createSegmentSwitchSection( this );
    GridData gridDataSegmentSwitch = new GridData( SWT.FILL, SWT.CENTER, true, false );
    segmentSwitchSection.setLayoutData( gridDataSegmentSwitch );

    /* Create profile control */
    final Control profilSection = createProfilSection( this );
    GridData gridDataProfileControl = new GridData( SWT.FILL, SWT.FILL, true, true );
    profilSection.setLayoutData( gridDataProfileControl );

    updateControl();
  }

  /**
   * in the profil section the crosssections will be displayed. the data filling is done in the function
   * "updateProfilSection" (see below)
   */
  private Control createProfilSection( Composite parent )
  {
    m_profilSection = new Section( parent, Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );

    return m_profilSection;
  }

  /**
   * in the segment section the crosssections will be displayed. the data filling is done in the function
   * "updateSegmentSection" (see below)
   */
  private Control createSegmentSwitchSection( Composite parent )
  {
    m_segmentSection = new Section( parent, Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );

    return m_segmentSection;
  }

  /**
   * in the segment section you can switch between the channel segments and convert the data into a model
   */
  private Control updateSegmentSwitchSection( )
  {
    final Control client = m_segmentSection.getClient();
    if( client != null && !client.isDisposed() )
      client.dispose();

    final Composite sectionClient = new Composite( m_segmentSection, SWT.NONE );
    sectionClient.setLayout( new GridLayout( 4, false ) );

    m_segmentSection.setClient( sectionClient );
    m_segmentSection.setText( "Segmentansicht" );
    if( m_data.getNumOfSegments() > 0 )
      m_segmentSection.setExpanded( true );
    else
      m_segmentSection.setExpanded( false );

    /* Header */
    final Label labelSpinnnerSegment = new Label( sectionClient, 0 );
    labelSpinnnerSegment.setText( "Wählen Sie das gewünschte Segment  ->" );
    GridData gridDataLabelSpinner = new GridData( SWT.FILL, SWT.CENTER, true, false );
    gridDataLabelSpinner.horizontalSpan = 1;
    labelSpinnnerSegment.setLayoutData( gridDataLabelSpinner );

    final Spinner spinnerSegment = new Spinner( sectionClient, 0 );
    GridData gridDataSegmentSpinner = new GridData( SWT.LEFT, SWT.CENTER, true, false );
    gridDataSegmentSpinner.horizontalSpan = 3;
    spinnerSegment.setLayoutData( gridDataSegmentSpinner );

    if( m_data.getNumOfSegments() > 1 )
    {
      spinnerSegment.setEnabled( true );
      spinnerSegment.setMinimum( 1 );
      spinnerSegment.setMaximum( m_data.getNumOfSegments() );

      if( m_data.getNumOfSegments() > 0 )
        spinnerSegment.setSelection( m_data.getSelectedSegment() );
      else
        spinnerSegment.setSelection( 0 );
    }
    else if( m_data.getNumOfSegments() == 1 )
    {
      spinnerSegment.setEnabled( false );
      spinnerSegment.setMinimum( 0 );
      spinnerSegment.setMaximum( m_data.getNumOfSegments() );
      spinnerSegment.setSelection( 1 );
    }
    else
      spinnerSegment.setEnabled( false );

    spinnerSegment.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent e )
      {
        m_data.setSelectedSegment( spinnerSegment.getSelection() );
        updateControl();
      }
    } );

    /* Group Segmentdaten */
    Group groupSegment = new Group( sectionClient, SWT.NULL );
    groupSegment.setText( "Segmentdaten" );

    GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 4;
    groupSegment.setLayout( gridLayout );
    GridData gridData = new GridData( SWT.FILL, SWT.CENTER, true, false );
    gridData.horizontalSpan = 4;
    groupSegment.setLayoutData( gridData );

    Composite compSegmentDataHeader = new Composite( groupSegment, SWT.NULL );
    GridLayout gridLayoutDataHeader = new GridLayout();
    gridLayoutDataHeader.numColumns = 2;
    compSegmentDataHeader.setLayout( gridLayoutDataHeader );
    GridData gridDataHeader = new GridData( SWT.FILL, SWT.CENTER, true, false );
    gridDataHeader.horizontalSpan = 4;
    compSegmentDataHeader.setLayoutData( gridDataHeader );

    Label labelNumIntersSegment = new Label( compSegmentDataHeader, SWT.NULL );
    labelNumIntersSegment.setText( "Anzahl der Uferlinienunterteilungen ->" );
    GridData gridDatalabel = new GridData( SWT.FILL, SWT.CENTER, true, false );
    gridDatalabel.horizontalSpan = 1;
    labelNumIntersSegment.setLayoutData( gridDatalabel );

    final Spinner spinnerNumIntersSegment = new Spinner( compSegmentDataHeader, 0 );

    GridData gridDataNumIntersSpinner = new GridData( SWT.LEFT, SWT.CENTER, true, false );
    gridDataNumIntersSpinner.horizontalSpan = 1;
    spinnerNumIntersSegment.setLayoutData( gridDataNumIntersSpinner );
    spinnerNumIntersSegment.setEnabled( true );
    spinnerNumIntersSegment.setMinimum( 2 );
    spinnerNumIntersSegment.setMaximum( 100 );
    if( m_data.getNumOfSegments() > 0 )
    {
      spinnerNumIntersSegment.setSelection( m_data.getNumBankIntersections( spinnerSegment.getSelection() ) );
    }
    else
    {
      spinnerNumIntersSegment.setEnabled( false );
      spinnerNumIntersSegment.setSelection( 0 );
    }
    spinnerNumIntersSegment.setToolTipText( "Wählen Sie die Anzahl der Uferlinienunterteilungen für das aktuelle Segment." );
    spinnerNumIntersSegment.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent e )
      {
        m_data.setNumBankIntersections( spinnerSegment.getSelection(), spinnerNumIntersSegment.getSelection() );
        updateControl();
      }
    } );

    Label labelProfile1 = new Label( groupSegment, SWT.NULL );
    labelProfile1.setText( "Profil 1" );

    Button buttonEditProf1 = new Button( groupSegment, SWT.PUSH );
    buttonEditProf1.setText( "edit" );
    buttonEditProf1.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        // bring profile 1 on the ChartView
      }
    } );

    Label labelBankline1 = new Label( groupSegment, SWT.NULL );
    labelBankline1.setText( "Uferlinie 1" );

    Button buttonEditBank1 = new Button( groupSegment, SWT.PUSH );
    buttonEditBank1.setText( "edit" );
    buttonEditBank1.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        // set LineString for Bank 1 editable
      }
    } );

    Label labelProfile2 = new Label( groupSegment, SWT.NULL );
    labelProfile2.setText( "Profil 2" );

    Button buttonEditProf2 = new Button( groupSegment, SWT.PUSH );
    buttonEditProf2.setText( "edit" );
    buttonEditProf2.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        // bring profile 2 on the ChartView
      }
    } );

    Label labelBankline2 = new Label( groupSegment, SWT.NULL );
    labelBankline2.setText( "Uferlinie 2" );

    Button buttonEditBank2 = new Button( groupSegment, SWT.PUSH );
    buttonEditBank2.setText( "edit" );
    buttonEditBank2.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        // set LineString for Bank 2 editable
      }
    } );

    /* conversion button */
    m_buttonConvertToModel = new Button( sectionClient, SWT.PUSH );
    m_buttonConvertToModel.setText( "alle Segmente ins Modell übernehmen..." );
    m_buttonConvertToModel.setToolTipText( "alle Segmente ins Modell übernehmen..." );
    m_buttonConvertToModel.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        m_data.convertToModel();
      }
    } );

    return m_segmentSection;
  }

  /**
   * in the bank section you can select / draw your banks (left / right) and specify the number of instersections (at
   * first global for all bank segments)
   */
  private Control createBankSelectionSection( Composite parent, IKalypsoFeatureTheme[] bankThemes )
  {
    Section bankSection = new Section( parent, Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    Composite sectionClient = new Composite( bankSection, SWT.NONE );
    sectionClient.setLayout( new GridLayout( 3, false ) );
    bankSection.setClient( sectionClient );
    bankSection.setText( "Uferlinien" );
    bankSection.setExpanded( true );
    bankSection.setDescription( "Wählen Sie Ihre Uferlinienthemen und ggf. hieraus einzelne Linien" );

    final ComboViewer combviewerBank1 = new ComboViewer( sectionClient, SWT.DROP_DOWN | SWT.READ_ONLY );
    combviewerBank1.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    combviewerBank1.setContentProvider( new ArrayContentProvider() );
    combviewerBank1.setLabelProvider( new LabelProvider() );
    final IKalypsoFeatureTheme bankTheme = m_data.getBankTheme();

    final IKalypsoFeatureTheme themeToBankSelect;
    if( bankThemes.length == 0 )
    {
      combviewerBank1.getControl().setEnabled( false );
      String msg = "<kein Uferlinienthema in Karte vorhanden>";
      combviewerBank1.setInput( new String[] { msg } );
      combviewerBank1.setSelection( new StructuredSelection( msg ) );
      themeToBankSelect = null;
    }
    else
    {
      combviewerBank1.setInput( bankThemes );

      if( bankTheme != null )
        themeToBankSelect = bankTheme;
      else
        themeToBankSelect = bankThemes[0];

      combviewerBank1.setSelection( new StructuredSelection( themeToBankSelect ) );
    }

    if( bankTheme != themeToBankSelect )
      m_data.setBankTheme( themeToBankSelect );

    combviewerBank1.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        m_data.setBankTheme( (IKalypsoFeatureTheme) selection.getFirstElement() );
      }
    } );

    /* Button for the first bank selection */
    final Button chooseFirstBankButton = new Button( sectionClient, SWT.TOGGLE );
    m_buttonList.add( chooseFirstBankButton );
    chooseFirstBankButton.setText( "Uferlinie 1 wählen..." );
    chooseFirstBankButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        if( chooseFirstBankButton.getSelection() )
        {
          buttonGuard( chooseFirstBankButton );
          final CreateChannelData.SIDE side = CreateChannelData.SIDE.LEFT;
          final IRectangleMapFunction function = new BankSelectorFunction( m_data, side );
          final IWidget selectBankWidget = new SelectionWidget( "", "", function );
          m_widget.setDelegate( selectBankWidget );
        }
        else
        {
          m_widget.setDelegate( null );
        }
      }
    } );

    /* Button for the first bank drawing */
    final Button drawFirstBankButton = new Button( sectionClient, SWT.TOGGLE );
    m_buttonList.add( drawFirstBankButton );
    drawFirstBankButton.setText( "zeichnen" );

    /* ComboBox for the second bank line theme selection */
    final ComboViewer combviewerBank2 = new ComboViewer( sectionClient, SWT.DROP_DOWN | SWT.READ_ONLY );
    combviewerBank2.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    combviewerBank2.setContentProvider( new ArrayContentProvider() );
    combviewerBank2.setLabelProvider( new LabelProvider() );
    final IKalypsoFeatureTheme bankTheme2 = m_data.getBankTheme();
    final IKalypsoFeatureTheme themeToBank2Select;
    if( bankThemes.length == 0 )
    {
      combviewerBank2.getControl().setEnabled( false );
      String msg = "<kein Uferlinienthema in Karte vorhanden>";
      combviewerBank2.setInput( new String[] { msg } );
      combviewerBank2.setSelection( new StructuredSelection( msg ) );
      themeToBank2Select = null;
    }
    else
    {
      combviewerBank2.setInput( bankThemes );

      if( bankTheme2 != null )
        themeToBank2Select = bankTheme2;
      else
        themeToBank2Select = bankThemes[0];

      combviewerBank2.setSelection( new StructuredSelection( themeToBank2Select ) );
    }

    if( bankTheme2 != themeToBank2Select )
      m_data.setBankTheme( themeToBank2Select );

    combviewerBank2.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        m_data.setBankTheme( (IKalypsoFeatureTheme) selection.getFirstElement() );
      }
    } );

    /* Button for the second bank selection */
    final Button chooseSecondBankButton = new Button( sectionClient, SWT.TOGGLE );
    m_buttonList.add( chooseSecondBankButton );
    chooseSecondBankButton.setText( "Uferlinie 2 wählen..." );
    chooseSecondBankButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        if( chooseSecondBankButton.getSelection() )
        {
          buttonGuard( chooseSecondBankButton );
          final CreateChannelData.SIDE side = CreateChannelData.SIDE.RIGHT;
          final IRectangleMapFunction function = new BankSelectorFunction( m_data, side );
          final IWidget selectBankWidget = new SelectionWidget( "", "", function );
          m_widget.setDelegate( selectBankWidget );
        }
        else
        {
          m_widget.setDelegate( null );
        }
      }
    } );

    /* Button for the second bank drawing */
    final Button drawSecondBankButton = new Button( sectionClient, SWT.TOGGLE );
    m_buttonList.add( drawSecondBankButton );
    drawSecondBankButton.setText( "zeichnen" );

    Composite compNumBankIntersections = new Composite( sectionClient, SWT.NONE );
    // Layout
    GridLayout gridLayoutNumBankInters = new GridLayout();
    gridLayoutNumBankInters.numColumns = 2;
    compNumBankIntersections.setLayout( gridLayoutNumBankInters );
    GridData gridDataNumProfInters = new GridData( SWT.FILL, SWT.CENTER, true, false );
    gridDataNumProfInters.horizontalSpan = 3;
    compNumBankIntersections.setLayoutData( gridDataNumProfInters );

    Label bankLabel = new Label( compNumBankIntersections, SWT.NULL );
    bankLabel.setText( "Anzahl der Stützstellen pro Uferlinie (global):" );
    final GridData gridData = new GridData();
    gridData.horizontalSpan = 1;
    bankLabel.setLayoutData( gridData );

    final Spinner spinNumBankIntersections = new Spinner( compNumBankIntersections, SWT.NONE );

    spinNumBankIntersections.setDigits( 0 );
    spinNumBankIntersections.setMinimum( 2 );
    spinNumBankIntersections.setMaximum( 100 );
    if( m_data.getGlobNumBankIntersections() == 0 )
      spinNumBankIntersections.setSelection( 6 );
    else
      spinNumBankIntersections.setSelection( m_data.getGlobNumBankIntersections() );
    m_data.setGlobNumBankIntersections( 6 );
    spinNumBankIntersections.setToolTipText( "Geben Sie hier die Anzahl der Stützstellen je Uferlinie ein. Diese Angabe gilt zunächst global für alle Segmente. Eine genaue Definition für jedes einzelne Segment kann in der Segmentansicht vorgenommen werden." );
    spinNumBankIntersections.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final int selection = spinNumBankIntersections.getSelection();
        m_data.setGlobNumBankIntersections( selection );
        updateControl();
      }
    } );

    final GridData gridDataSpinner = new GridData();
    gridDataSpinner.horizontalAlignment = SWT.END;
    spinNumBankIntersections.setLayoutData( gridDataSpinner );

    return bankSection;
  }

  /**
   * in the profile section you can select / draw your profiles (WSPM-profiles) and specify the number of instersections
   * (global for all profiles)
   */
  private Control createProfileSelectionSection( final Composite parent, IKalypsoFeatureTheme[] profileThemes )
  {
    /* profile selection expandable section */
    Section mysection = new Section( parent, Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    Composite sectionClient = new Composite( mysection, SWT.NONE );
    sectionClient.setLayout( new GridLayout( 2, false ) );
    mysection.setClient( sectionClient );
    mysection.setText( "Profile" );
    mysection.setExpanded( true );
    mysection.setDescription( "Wählen Sie Ihr Profilthema und ggf. hieraus einzelne Profile" );

    /* add combo-box for the wspm-profile theme selection */
    final ComboViewer combviewerProfiles = new ComboViewer( sectionClient, SWT.DROP_DOWN | SWT.READ_ONLY );
    combviewerProfiles.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    combviewerProfiles.setContentProvider( new ArrayContentProvider() );
    combviewerProfiles.setLabelProvider( new LabelProvider() );

    final IKalypsoFeatureTheme profileTheme = m_data.getProfileTheme();

    final IKalypsoFeatureTheme themeToProfileSelect;
    if( profileThemes.length == 0 )
    {
      combviewerProfiles.getControl().setEnabled( false );
      String msg = "<kein Profilthema in Karte vorhanden>";
      combviewerProfiles.setInput( new String[] { msg } );
      combviewerProfiles.setSelection( new StructuredSelection( msg ) );
      themeToProfileSelect = null;
    }
    else
    {
      combviewerProfiles.setInput( profileThemes );

      if( profileTheme != null )
        themeToProfileSelect = profileTheme;
      else
        themeToProfileSelect = profileThemes[0];

      combviewerProfiles.setSelection( new StructuredSelection( themeToProfileSelect ) );
    }

    if( profileTheme != themeToProfileSelect )
      m_data.setProfileTheme( themeToProfileSelect );

    combviewerProfiles.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        m_data.setProfileTheme( (IKalypsoFeatureTheme) selection.getFirstElement() );
      }
    } );

    /* Button for the wspm-profile selection */
    GridData gridData = new GridData();
    gridData.horizontalAlignment = SWT.FILL;

    final Button chooseProfilesButton = new Button( sectionClient, SWT.TOGGLE );
    m_buttonList.add( chooseProfilesButton );
    chooseProfilesButton.setLayoutData( gridData );
    chooseProfilesButton.setText( "Profile wählen..." );
    chooseProfilesButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        if( chooseProfilesButton.getSelection() )
        {
          buttonGuard( chooseProfilesButton );
          final IRectangleMapFunction function = new ProfileSelectorFunction( m_data );
          final IWidget selectProfileWidget = new SelectionWidget( "", "", function );
          m_widget.setDelegate( selectProfileWidget );
        }
        else
        {
          m_widget.setDelegate( null );
        }
      }
    } );

    new Label( sectionClient, SWT.NULL ).setText( "Anzahl der Stützstellen pro Profil (global):" );
    final Spinner spinNumProfIntersections = new Spinner( sectionClient, SWT.NONE );

    spinNumProfIntersections.setDigits( 0 );
    spinNumProfIntersections.setMinimum( 2 );
    spinNumProfIntersections.setMaximum( 100 );
    if( m_data.getNumProfileIntersections() == 0 )
      spinNumProfIntersections.setSelection( 6 );
    else
      spinNumProfIntersections.setSelection( m_data.getNumProfileIntersections() );
    spinNumProfIntersections.setToolTipText( "Geben Sie hier die Anzahl der Stützstellen je Profil ein." );

    spinNumProfIntersections.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        m_data.setNumProfileIntersections( spinNumProfIntersections.getSelection() );

        updateControl();
      }
    } );

    final GridData gridDataSpinner = new GridData();
    gridDataSpinner.horizontalAlignment = SWT.END;
    spinNumProfIntersections.setLayoutData( gridDataSpinner );
    m_data.setNumProfileIntersections( spinNumProfIntersections.getSelection() );

    return mysection;
  }

  public void updateControl( )
  {

    updateSegmentSwitchSection();
    m_data.updateSegments();
    updateProfilSection();
    m_buttonConvertToModel.setEnabled( m_data.getMeshStatus() );
  }

  /**
   * displays the selected crosssections -> data filling for the profil section
   */
  private void updateProfilSection( )
  {
    final Control client = m_profilSection.getClient();
    if( client != null && !client.isDisposed() )
      client.dispose();

    final Composite sectionClient = new Composite( m_profilSection, SWT.NONE );
    sectionClient.setLayout( new GridLayout( 1, false ) );

    m_profilSection.setClient( sectionClient );
    m_profilSection.setText( "Profilansicht" );
    m_profilSection.setDescription( "Bearbeiten Sie die Profilunterteilungen" );

    final IProfilEventManager pem = m_data.getProfilEventManager();
    final IProfil profil = pem.getProfil();
    if( profil == null )
    {
      final Label label = new Label( sectionClient, SWT.NONE );
      label.setText( "Kein Profil selektiert" );
    }
    else
    {
      final ProfilChartView profilChartView = new ProfilChartView( pem, m_viewData, m_colorRegistry );

      final ToolBarManager manager = new ToolBarManager( SWT.HORIZONTAL );
      manager.createControl( sectionClient );

      final Control profilControl = profilChartView.createControl( sectionClient, SWT.BORDER );
      profilControl.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

      for( final ProfilChartActionsEnum action : ProfilChartActionsEnum.values() )
        manager.add( ProfilChartActionsEnum.createAction( profilChartView, action ) );

      manager.update( true );
    }

    m_profilSection.setExpanded( profil != null );
  }

  /**
   * simulates a radio button set
   */
  private void buttonGuard( Button activatedButton )
  {
    for( int i = 0; i < m_buttonList.size(); i++ )
    {
      final Button currentButton = m_buttonList.get( i );
      if( !activatedButton.equals( currentButton ) )
      {
        currentButton.setSelection( false );
      }
    }
  }

  public void setConversionButton( )
  {

    Button button = m_buttonList.get( getStyle() );
    for( int i = 0; i < m_buttonList.size(); i++ )
    {
      final Button currentButton = m_buttonList.get( i );
      if( button.equals( currentButton ) )
      {
        currentButton.setEnabled( true );
      }
    }
  }
}
