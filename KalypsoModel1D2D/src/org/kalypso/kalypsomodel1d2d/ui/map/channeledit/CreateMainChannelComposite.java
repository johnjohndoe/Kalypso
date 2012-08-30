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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.layout.GridLayoutFactory;
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
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.menus.CommandContributionItem;
import org.kalypso.chart.ui.editor.commandhandler.ChartSourceProvider;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.kalypso.commons.eclipse.ui.EmbeddedSourceToolbarManager;
import org.kalypso.contribs.eclipse.jface.action.ActionButton;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DUIImages;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.PROF;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.overlay.IWspmOverlayConstants;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.overlay.ProfilOverlayLayer;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.overlay.ProfilOverlayLayerProvider;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.widgets.SelectionWidget;
import org.kalypso.ogc.gml.map.widgets.mapfunctions.IRectangleMapFunction;
import org.kalypso.ogc.gml.mapmodel.KalypsoFeatureThemeHelper;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypsodeegree.model.geometry.GM_Envelope;

import de.openali.odysseus.chart.framework.model.IChartModel;
import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.model.layer.ILayerManager;
import de.openali.odysseus.chart.framework.util.ChartUtilities;
import de.openali.odysseus.chart.framework.view.IChartComposite;

/**
 * @author Thomas Jung
 */
public class CreateMainChannelComposite extends Composite
{
  private static final int SPINNER_WIDTH = 50;

  private final CreateChannelData m_data;

  private final CreateMainChannelWidget m_widget;

  private Section m_profilSection;

  private Section m_segmentSection;

  private boolean m_buttonStateZoom;

  /*********************************************************************************************************************
   * m_buttonList<BR>
   * Following buttons are present in the Schlauchgenerator: <BR>
   * Profile w‰hlen; Uferlinie 1 w‰hlen; Uferlinie 1 zeichnen; Uferlinie 2 w‰hlen; Uferlinie 2 zeichnen:
   */
  private final List<Button> m_buttonList = new ArrayList<Button>();

  private Button m_buttonConvertToModel;

  private final FormToolkit m_toolkit;

  private Button m_buttonEditBank;

  private EmbeddedSourceToolbarManager m_sourceManager;

  public CreateMainChannelComposite( final Composite parent, final FormToolkit toolkit, final int style, final CreateChannelData data, final CreateMainChannelWidget widget )
  {
    super( parent, style );

    data.setShell( getShell() );

    m_toolkit = toolkit;
    m_toolkit.adapt( this );

    m_data = data;
    m_widget = widget;

    setLayout( new FillLayout() );

    createContents();
  }

  /**
   * initialisation
   */
  private void createContents( )
  {
    final ScrolledForm form = m_toolkit.createScrolledForm( this );
    final Composite body = form.getBody();
    GridLayoutFactory.fillDefaults().applyTo( body );

    /* Create Profile control */
    final IKalypsoFeatureTheme[] profileThemes = m_data.getProfileThemes();
    final Control profileSection = createProfileSelectionSection( body, profileThemes );
    final GridData gridDataProf = new GridData( SWT.FILL, SWT.CENTER, true, false );
    profileSection.setLayoutData( gridDataProf );

    /* Create Bank control */
    final IKalypsoFeatureTheme[] bankThemes = KalypsoFeatureThemeHelper.getLineThemes( m_widget.getMapPanel() );
    final Control bankSection = createBankSelectionSection( body, bankThemes );
    final GridData gridDataBank = new GridData( SWT.FILL, SWT.CENTER, true, false );
    bankSection.setLayoutData( gridDataBank );

    /* Create segment switch control */
    final Control segmentSwitchSection = createSegmentSwitchSection( body );
    final GridData gridDataSegmentSwitch = new GridData( SWT.FILL, SWT.CENTER, true, false );
    segmentSwitchSection.setLayoutData( gridDataSegmentSwitch );

    /* Create profile control */
    final Control profilSection = createProfilSection( body );
    final GridData gridDataProfileControl = new GridData( SWT.FILL, SWT.FILL, true, true );
    profilSection.setLayoutData( gridDataProfileControl );

    /* conversion to model composite */
    final Composite compConversion = m_toolkit.createComposite( body, SWT.NONE );
    compConversion.setLayout( new GridLayout( 2, false ) );

    final CreateMainChannelApplyAction applyToAction = new CreateMainChannelApplyAction( m_data, this );
    final Button applyToButton = ActionButton.createButton( m_toolkit, compConversion, applyToAction );
    applyToButton.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );

    updateControl( true );

    if( m_data.isBankEdit() )
    {
      m_buttonEditBank.setSelection( true );
      editButtonUpdateBank();
    }
  }

  /**
   * in the profil section the crosssections will be displayed. the data filling is done in the function
   * "updateProfilSection" (see below)
   */
  private Control createProfilSection( final Composite parent )
  {
    m_profilSection = m_toolkit.createSection( parent, Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );

    m_profilSection.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.39" ) ); //$NON-NLS-1$
    m_profilSection.setDescription( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.40" ) ); //$NON-NLS-1$

    return m_profilSection;
  }

  /**
   * in the segment section the crosssections will be displayed. the data filling is done in the function
   * "updateSegmentSection" (see below)
   */
  private Control createSegmentSwitchSection( final Composite parent )
  {
    m_segmentSection = m_toolkit.createSection( parent, Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );

    m_segmentSection.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.0" ) ); //$NON-NLS-1$
    m_segmentSection.setDescription( "Bearbeiten Sie einen Profilabschnitt (Segment zwischen zwei Profilen)" );

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

    final Composite sectionClient = m_toolkit.createComposite( m_segmentSection, SWT.NONE );
    GridLayoutFactory.fillDefaults().applyTo( sectionClient );
    m_segmentSection.setClient( sectionClient );

    if( m_data.getNumOfSegments() > 0 )
      m_segmentSection.setExpanded( true );
    else
      m_segmentSection.setExpanded( false );

    /** ************************ Header ***************************** */

    final Group headerGroup = new Group( sectionClient, SWT.NONE );
    m_toolkit.adapt( headerGroup );
    headerGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( headerGroup );

    headerGroup.setText( "Segmentauswahl" );

    /* label */
    final Label labelSpinnnerSegment = m_toolkit.createLabel( headerGroup, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.1" ) ); //$NON-NLS-1$
    labelSpinnnerSegment.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    /* spinner for specify the current segment */
    final Spinner spinnerSegment = new Spinner( headerGroup, SWT.BORDER );
    m_toolkit.adapt( spinnerSegment );
    final GridData gridDataSegmentSpinner = new GridData( SWT.END, SWT.CENTER, true, false );
    gridDataSegmentSpinner.widthHint = SPINNER_WIDTH;
    spinnerSegment.setLayoutData( gridDataSegmentSpinner );

    /* zoom to extend button */
    final Button buttonZoomToExtend = new Button( headerGroup, SWT.CHECK );
    buttonZoomToExtend.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
    buttonZoomToExtend.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.2" ) ); //$NON-NLS-1$
    buttonZoomToExtend.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.43" ) ); //$NON-NLS-1$
    buttonZoomToExtend.setSelection( m_buttonStateZoom );
    buttonZoomToExtend.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        if( m_buttonStateZoom == false )
        {
          m_buttonStateZoom = true;

          final GM_Envelope mapExtend = m_data.getSelectedSegment().getSegmentMapExtend();
          if( mapExtend != null )
          {
            final IMapPanel panel = m_widget.getMapPanel();
            panel.setBoundingBox( mapExtend );
          }
        }
        else
          m_buttonStateZoom = false;
      }
    } );

    if( m_data.getNumOfSegments() > 1 )
    {
      spinnerSegment.setEnabled( true );
      spinnerSegment.setMinimum( 1 );
      spinnerSegment.setMaximum( m_data.getNumOfSegments() );
      if( m_data.getSelectedSegment() == null )
        spinnerSegment.setSelection( 1 );
      else
        spinnerSegment.setSelection( m_data.getSelectedSegmentPos() + 1 );

      /* initial selection */
    }
    else if( m_data.getNumOfSegments() == 1 )
    {
      spinnerSegment.setEnabled( false );
      spinnerSegment.setMinimum( 1 );
      spinnerSegment.setMaximum( m_data.getNumOfSegments() );
      spinnerSegment.setSelection( 1 );
    }
    else
      spinnerSegment.setEnabled( false );

    spinnerSegment.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        m_data.setSelectedSegment( spinnerSegment.getSelection() - 1 );

        if( buttonZoomToExtend.getSelection() == true )
        {
          final GM_Envelope mapExtend = m_data.getSelectedSegment().getSegmentMapExtend();
          if( mapExtend != null )
          {
            final IMapPanel panel = m_widget.getMapPanel();
            panel.setBoundingBox( mapExtend );
            if( m_buttonEditBank.getSelection() == true )
              editButtonUpdateBank();

            panel.repaintMap();
          }
        }
        if( m_buttonEditBank.getSelection() == true )
          editButtonUpdateBank();

        updateControl( true );
      }
    } );

    /* Group Segmentdaten */
    final Group groupSegment = new Group( sectionClient, SWT.NULL );
    groupSegment.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( groupSegment );

    groupSegment.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.3" ) ); //$NON-NLS-1$

    final Label labelNumIntersSegment = m_toolkit.createLabel( groupSegment, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.4" ) ); //$NON-NLS-1$
    labelNumIntersSegment.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    /* spinner for specifiying the number of intersection points for the current segment */
    final Spinner spinnerNumIntersSegment = new Spinner( groupSegment, SWT.BORDER );
    m_toolkit.adapt( spinnerNumIntersSegment );

    final GridData gridDataNumIntersSpinner = new GridData( SWT.RIGHT, SWT.CENTER, true, false );
    gridDataNumIntersSpinner.widthHint = SPINNER_WIDTH;
    spinnerNumIntersSegment.setLayoutData( gridDataNumIntersSpinner );

    spinnerNumIntersSegment.setEnabled( true );
    spinnerNumIntersSegment.setMinimum( 2 );
    spinnerNumIntersSegment.setMaximum( 999 );
    if( m_data.getNumOfSegments() > 0 )
    {
      spinnerNumIntersSegment.setSelection( m_data.getNumBankIntersections( m_data.getSegmentAtListPos( spinnerSegment.getSelection() - 1 ) ) );
    }
    else
    {
      spinnerNumIntersSegment.setEnabled( false );
      spinnerNumIntersSegment.setSelection( 0 );
    }
    spinnerNumIntersSegment.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.5" ) ); //$NON-NLS-1$
    spinnerNumIntersSegment.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        m_data.setNumBankIntersections( spinnerSegment.getSelection(), spinnerNumIntersSegment.getSelection() );
        // just update the selected segment
        m_data.updateSegment( false );
        if( m_buttonEditBank.getSelection() == true )
          editButtonUpdateBank();

        m_widget.getMapPanel().repaintMap();
      }
    } );

    final Label labelBankline = m_toolkit.createLabel( groupSegment, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.8" ) ); //$NON-NLS-1$
    labelBankline.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    /* edit button for bankline 1 */
    m_buttonEditBank = m_toolkit.createButton( groupSegment, "", SWT.TOGGLE ); //$NON-NLS-1$
    m_buttonEditBank.setLayoutData( new GridData( SWT.END, SWT.CENTER, true, false ) );

    m_buttonEditBank.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.9" ) ); //$NON-NLS-1$

    final PluginImageProvider imageProvider = KalypsoModel1D2DPlugin.getImageProvider();
    final Image editImage = imageProvider.getImage( KalypsoModel1D2DUIImages.IMGKEY.EDIT );
    m_buttonEditBank.setImage( editImage );
    m_buttonEditBank.setSelection( m_data.isBankEdit() );
    m_buttonEditBank.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        editButtonUpdateBank();
        resetButtonGuard();
        m_widget.getMapPanel().repaintMap();
      }
    } );

    return m_segmentSection;
  }

  /**
   * in the bank section you can select / draw your banks (left / right) and specify the number of instersections (at
   * first global for all bank segments)
   */
  private Control createBankSelectionSection( final Composite parent, final IKalypsoFeatureTheme[] bankThemes )
  {
    final Section bankSection = m_toolkit.createSection( parent, Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );

    final Composite sectionClient = m_toolkit.createComposite( bankSection, SWT.NONE );
    sectionClient.setLayout( new GridLayout( 3, false ) );
    bankSection.setClient( sectionClient );

    bankSection.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.16" ) ); //$NON-NLS-1$
    bankSection.setExpanded( true );
    bankSection.setDescription( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.17" ) ); //$NON-NLS-1$

    final ComboViewer combviewerBank1 = new ComboViewer( sectionClient, SWT.DROP_DOWN | SWT.READ_ONLY );
    m_toolkit.adapt( combviewerBank1.getControl(), true, false );
    combviewerBank1.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    combviewerBank1.setContentProvider( new ArrayContentProvider() );
    combviewerBank1.setLabelProvider( new LabelProvider() );
    final IKalypsoFeatureTheme bankTheme = m_data.getBankTheme1();

    final IKalypsoFeatureTheme themeToBankSelect;
    if( bankThemes.length == 0 )
    {
      combviewerBank1.getControl().setEnabled( false );
      final String msg = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.18" ); //$NON-NLS-1$
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
      m_data.setBankTheme1( themeToBankSelect );

    combviewerBank1.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        m_data.setBankTheme1( (IKalypsoFeatureTheme) selection.getFirstElement() );
      }
    } );

    /* Button for the first bank selection */
    final Button chooseFirstBankButton = m_toolkit.createButton( sectionClient, "", SWT.TOGGLE ); //$NON-NLS-1$
    m_buttonList.add( chooseFirstBankButton );
    chooseFirstBankButton.setLayoutData( new GridData( SWT.END, SWT.CENTER, false, false ) );

    chooseFirstBankButton.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.19" ) ); //$NON-NLS-1$
    final PluginImageProvider imageProvider = KalypsoModel1D2DPlugin.getImageProvider();
    final Image selImage = imageProvider.getImage( KalypsoModel1D2DUIImages.IMGKEY.SELECT );

    chooseFirstBankButton.setImage( selImage );
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
          final IWidget selectBankWidget = new SelectionWidget( "", "", function ); //$NON-NLS-1$ //$NON-NLS-2$
          m_widget.setDelegate( selectBankWidget );
        }
        else
        {
          m_widget.setDelegate( null );
        }
      }
    } );

    /* Button for the first bank drawing */
    final Button drawFirstBankButton = m_toolkit.createButton( sectionClient, "", SWT.TOGGLE ); //$NON-NLS-1$
    m_buttonList.add( drawFirstBankButton );
    drawFirstBankButton.setLayoutData( new GridData( SWT.END, SWT.CENTER, false, false ) );

    drawFirstBankButton.setVisible( true );
    final Image editImage = imageProvider.getImage( KalypsoModel1D2DUIImages.IMGKEY.EDIT );
    drawFirstBankButton.setImage( editImage );
    drawFirstBankButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        if( drawFirstBankButton.getSelection() )
        {
          buttonGuard( drawFirstBankButton );
          final CreateChannelData.SIDE side = CreateChannelData.SIDE.LEFT;
          final IWidget drawBankWidget = new DrawBanklineWidget( m_data, side, "", "" ); //$NON-NLS-1$ //$NON-NLS-2$
          m_widget.setDelegate( drawBankWidget );
        }
        else
        {
          m_widget.setDelegate( null );
        }
      }
    } );

    /* ComboBox for the second bank line theme selection */
    final ComboViewer combviewerBank2 = new ComboViewer( sectionClient, SWT.DROP_DOWN | SWT.READ_ONLY );
    m_toolkit.adapt( combviewerBank2.getControl(), true, false );
    combviewerBank2.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    combviewerBank2.setContentProvider( new ArrayContentProvider() );
    combviewerBank2.setLabelProvider( new LabelProvider() );
    final IKalypsoFeatureTheme bankTheme2 = m_data.getBankTheme2();
    final IKalypsoFeatureTheme themeToBank2Select;
    if( bankThemes.length == 0 )
    {
      combviewerBank2.getControl().setEnabled( false );
      final String msg = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.23" ); //$NON-NLS-1$
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
      m_data.setBankTheme2( themeToBank2Select );

    combviewerBank2.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        m_data.setBankTheme2( (IKalypsoFeatureTheme) selection.getFirstElement() );
      }
    } );

    /* Button for the second bank selection */
    final Button chooseSecondBankButton = m_toolkit.createButton( sectionClient, StringUtils.EMPTY, SWT.TOGGLE );
    chooseSecondBankButton.setLayoutData( new GridData( SWT.END, SWT.CENTER, false, false ) );

    m_buttonList.add( chooseSecondBankButton );

    chooseSecondBankButton.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.24" ) ); //$NON-NLS-1$
    chooseSecondBankButton.setImage( selImage );
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
          final IWidget selectBankWidget = new SelectionWidget( "", "", function ); //$NON-NLS-1$ //$NON-NLS-2$
          m_widget.setDelegate( selectBankWidget );
        }
        else
        {
          m_widget.setDelegate( null );
        }
      }
    } );

    /* Button for the second bank drawing */
    final Button drawSecondBankButton = m_toolkit.createButton( sectionClient, "", SWT.TOGGLE ); //$NON-NLS-1$
    m_buttonList.add( drawSecondBankButton );
    drawSecondBankButton.setLayoutData( new GridData( SWT.END, SWT.CENTER, false, false ) );

    drawSecondBankButton.setVisible( true );
    drawSecondBankButton.setImage( editImage );
    drawSecondBankButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        if( drawSecondBankButton.getSelection() )
        {
          buttonGuard( drawSecondBankButton );
          final CreateChannelData.SIDE side = CreateChannelData.SIDE.RIGHT;
          final IWidget drawBankWidget = new DrawBanklineWidget( m_data, side, "", "" ); //$NON-NLS-1$ //$NON-NLS-2$
          m_widget.setDelegate( drawBankWidget );
        }
        else
        {
          m_widget.setDelegate( null );
        }
      }
    } );

    /* spinner for number for bank parts */
    final Label bankLabel = new Label( sectionClient, SWT.NULL );
    bankLabel.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.28" ) ); //$NON-NLS-1$
    bankLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Spinner spinNumBankIntersections = new Spinner( sectionClient, SWT.BORDER );

    final GridData gridDataSpin = new GridData( SWT.FILL, SWT.CENTER, false, false, 2, 1 );
    gridDataSpin.widthHint = SPINNER_WIDTH;
    spinNumBankIntersections.setLayoutData( gridDataSpin );

    spinNumBankIntersections.setDigits( 0 );
    spinNumBankIntersections.setMinimum( 2 );
    spinNumBankIntersections.setMaximum( 999 );

    if( m_data.getGlobNumBankIntersections() == 0 )
      spinNumBankIntersections.setSelection( 6 );
    else
      spinNumBankIntersections.setSelection( m_data.getGlobNumBankIntersections() );

    m_data.setGlobNumBankIntersections( 6 );

    spinNumBankIntersections.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.29" ) ); //$NON-NLS-1$
    spinNumBankIntersections.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        if( m_data.getMeshStatus() == true )
        {
          if( !MessageDialog.openQuestion( getShell(), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.50" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.53" ) ) ) //$NON-NLS-1$ //$NON-NLS-2$
          {
            spinNumBankIntersections.setSelection( m_data.getGlobNumBankIntersections() );
            return;
          }
        }
        final int selection = spinNumBankIntersections.getSelection();
        m_data.setGlobNumBankIntersections( selection );
        updateControl( false );
      }
    } );

    return bankSection;
  }

  /**
   * in the profile section you can select / draw your profiles (WSPM-profiles) and specify the number of instersections
   * (global for all profiles)
   */
  private Control createProfileSelectionSection( final Composite parent, final IKalypsoFeatureTheme[] profileThemes )
  {
    /* profile selection expandable section */
    final Section mysection = m_toolkit.createSection( parent, Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    final Composite sectionClient = m_toolkit.createComposite( mysection, SWT.NONE );
    sectionClient.setLayout( new GridLayout( 2, false ) );
    mysection.setClient( sectionClient );
    mysection.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.30" ) ); //$NON-NLS-1$
    mysection.setExpanded( true );
    mysection.setDescription( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.31" ) ); //$NON-NLS-1$

    /* add combo-box for the wspm-profile theme selection */
    final ComboViewer combviewerProfiles = new ComboViewer( sectionClient, SWT.DROP_DOWN | SWT.READ_ONLY );
    m_toolkit.adapt( combviewerProfiles.getControl(), true, false );
    combviewerProfiles.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    combviewerProfiles.setContentProvider( new ArrayContentProvider() );
    combviewerProfiles.setLabelProvider( new LabelProvider() );

    final IKalypsoFeatureTheme profileTheme = m_data.getProfileTheme();

    final IKalypsoFeatureTheme themeToProfileSelect;
    if( profileThemes.length == 0 )
    {
      combviewerProfiles.getControl().setEnabled( false );
      final String msg = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.32" ); //$NON-NLS-1$
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
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        m_data.setProfileTheme( (IKalypsoFeatureTheme) selection.getFirstElement() );
      }
    } );

    final GridData gridData = new GridData();
    gridData.horizontalAlignment = SWT.RIGHT;

    /* Button for the wspm-profile selection */
    final Button chooseProfilesButton = m_toolkit.createButton( sectionClient, StringUtils.EMPTY, SWT.TOGGLE );
    m_buttonList.add( chooseProfilesButton );
    final GridData layoutDataFirstButton = new GridData( SWT.LEFT, SWT.CENTER, false, false );
    layoutDataFirstButton.minimumWidth = 25;
    chooseProfilesButton.setLayoutData( layoutDataFirstButton );
    chooseProfilesButton.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.33" ) ); //$NON-NLS-1$

    final PluginImageProvider imageProvider = KalypsoModel1D2DPlugin.getImageProvider();
    final Image selImage = imageProvider.getImage( KalypsoModel1D2DUIImages.IMGKEY.SELECT );

    chooseProfilesButton.setImage( selImage );
    chooseProfilesButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        if( chooseProfilesButton.getSelection() )
        {
          buttonGuard( chooseProfilesButton );
          final IRectangleMapFunction function = new ProfileSelectorFunction( m_data );
          final IWidget selectProfileWidget = new SelectionWidget( "", "", function ); //$NON-NLS-1$ //$NON-NLS-2$
          m_widget.setDelegate( selectProfileWidget );
        }
        else
        {
          m_widget.setDelegate( null );
        }
      }
    } );
    /* spinner for specifying the global number of profile intersection points */
    final Label spinnerLabel = new Label( sectionClient, SWT.NULL );
    spinnerLabel.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.36" ) ); //$NON-NLS-1$
    spinnerLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Spinner spinNumProfIntersections = new Spinner( sectionClient, SWT.BORDER );
    m_toolkit.adapt( spinNumProfIntersections );

    final GridData gridDataSpinner = new GridData( SWT.FILL, SWT.CENTER, false, false );
    gridDataSpinner.widthHint = SPINNER_WIDTH;
    spinNumProfIntersections.setLayoutData( gridDataSpinner );

    spinNumProfIntersections.setDigits( 0 );
    spinNumProfIntersections.setMinimum( 4 );
    spinNumProfIntersections.setMaximum( 999 );
    if( m_data.getNumProfileIntersections() == 0 )
      spinNumProfIntersections.setSelection( 6 );
    else
      spinNumProfIntersections.setSelection( m_data.getNumProfileIntersections() );
    spinNumProfIntersections.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.37" ) ); //$NON-NLS-1$
    spinNumProfIntersections.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        if( m_data.getMeshStatus() == true )
        {
          if( !MessageDialog.openQuestion( getShell(), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.52" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.53" ) ) ) //$NON-NLS-1$ //$NON-NLS-2$
          {
            spinNumProfIntersections.setSelection( m_data.getNumProfileIntersections() );
            return;
          }
        }

        m_data.setNumProfileIntersections( spinNumProfIntersections.getSelection() );
        updateControl( false );
      }
    } );

    m_data.setNumProfileIntersections( spinNumProfIntersections.getSelection() );

    return mysection;
  }

  /**
   * updates the composite controls and the data (if edit == false)
   */
  public void updateControl( final boolean edit )
  {
    try
    {
      updateSegmentSwitchSection();
      m_data.updateSegments( edit );
      updateProfilSection();
      m_buttonConvertToModel.setEnabled( m_data.getMeshStatus() );
      this.layout();
    }
    catch( final Throwable t )
    {
      t.printStackTrace();
    }

  }

  /**
   * updates the data for all segments
   */
  public void updateData( final boolean edit )
  {
    m_data.updateSegments( edit );
  }

  /**
   * displays the selected cross sections -> data filling for the profil section
   */
  private void updateProfilSection( )
  {
    if( m_sourceManager != null )
      m_sourceManager.dispose();

    final Control client = m_profilSection.getClient();
    if( client != null && !client.isDisposed() )
      client.dispose();

    final Composite sectionClient = m_toolkit.createComposite( m_profilSection, SWT.NONE );
    GridLayoutFactory.fillDefaults().spacing( 0, 0 ).applyTo( sectionClient );
    m_profilSection.setClient( sectionClient );

    m_widget.getMapPanel().repaintMap();

    final IProfil profil = m_data.getProfil();
    if( profil == null )
    {
      final Label label = m_toolkit.createLabel( sectionClient, StringUtils.EMPTY, SWT.NONE );
      label.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.41" ) ); //$NON-NLS-1$
    }
    else
    {
      // init while first load
      if( m_data.getCurrentProfile() == null )
      {
        final double station = profil.getStation();
        final CreateChannelData.PROF profPlace = m_data.getCurrentProfilePlace( station );
        m_data.setCurrentProfile( profPlace );
        // here repaint!!
      }
      final SegmentData currentSegment = m_data.getSelectedSegment();
      final ProfilChartView profilChartView = new ProfilChartView();
      final IProfilLayerProvider layerProvider = new ProfilOverlayLayerProvider();
      profilChartView.setProfil( profil, null );
      profilChartView.setLayerProvider( layerProvider );

      final String stationText = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.11", profil.getStation() ); //$NON-NLS-1$
      final Label label = m_toolkit.createLabel( sectionClient, stationText, SWT.BORDER | SWT.CENTER );
      label.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
      label.setBackground( label.getDisplay().getSystemColor( SWT.COLOR_WHITE ) );

      final ToolBarManager manager = new ToolBarManager( SWT.HORIZONTAL | SWT.FLAT | SWT.SHADOW_OUT );
      manager.createControl( sectionClient ).setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

      final Control profilControl = profilChartView.createControl( sectionClient );
      profilControl.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

      final IChartComposite chart = profilChartView.getChartComposite();
      final IChartModel chartModel = chart.getChartModel();
      final ILayerManager mngr = chartModel.getLayerManager();
      final IChartLayer overlayLayer = mngr.findLayer( IWspmOverlayConstants.LAYER_OVERLAY );

      if( overlayLayer instanceof ProfilOverlayLayer )
      {
        if( currentSegment != null )
        {
          final IProfil layerData;
          if( m_data.getCurrentProfile() == PROF.UP )
            layerData = currentSegment.getProfUpIntersProfile();
          else
            layerData = currentSegment.getProfDownIntersProfile();

          ((ProfilOverlayLayer) overlayLayer).setProfile( layerData, m_data, m_widget );
          m_widget.getMapPanel().repaintMap();
        }
      }

      m_sourceManager = new EmbeddedSourceToolbarManager( PlatformUI.getWorkbench(), ChartSourceProvider.ACTIVE_CHART_NAME, chart );
      final Map<String, Integer> commands = new LinkedHashMap<String, Integer>();
      commands.put( "org.kalypso.chart.ui.commands.zoom_pan_maximize", CommandContributionItem.STYLE_RADIO ); //$NON-NLS-1$
      commands.put( "org.kalypso.chart.ui.commands.pan", CommandContributionItem.STYLE_RADIO ); //$NON-NLS-1$
      commands.put( "org.kalypso.chart.ui.commands.edit", CommandContributionItem.STYLE_RADIO ); //$NON-NLS-1$
      commands.put( StringUtils.EMPTY, 0 );
      commands.put( "org.kalypso.chart.ui.commands.maximize", CommandContributionItem.STYLE_PUSH ); //$NON-NLS-1$
      commands.put( "org.kalypso.chart.ui.commands.ExportClipboardCommand", CommandContributionItem.STYLE_PUSH ); //$NON-NLS-1$
      commands.put( "org.kalypso.chart.ui.editor.commandhandler.ExportHandler", CommandContributionItem.STYLE_PUSH ); //$NON-NLS-1$
      m_sourceManager.fillToolbar( manager, commands );

      manager.add( new SwitchProfileAction( this ) );

      manager.update( true );

      ChartUtilities.maximize( chartModel );
    }

    m_profilSection.setExpanded( profil != null );
  }

  void buttonGuard( final Button activatedButton )
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

  void resetButtonGuard( )
  {
    for( int i = 0; i < m_buttonList.size(); i++ )
    {
      final Button currentButton = m_buttonList.get( i );
      if( currentButton.getSelection() == true )
      {
        currentButton.setSelection( false );
      }
    }
    // TODO: manage to switch off the edit bank line
    // m_bankEdit1 = false;
    // m_bankEdit2 = false;
    // m_buttonEditBank1.setSelection( false );
    // m_buttonEditBank2.setSelection( false );
  }

  public void setConversionButton( )
  {
    final Button button = m_buttonList.get( getStyle() );
    for( int i = 0; i < m_buttonList.size(); i++ )
    {
      final Button currentButton = m_buttonList.get( i );
      if( button.equals( currentButton ) )
      {
        currentButton.setEnabled( true );
      }
    }
  }

  void editButtonUpdateBank( )
  {
    if( m_buttonEditBank.getSelection() == true )
    {
      m_data.setBankEdit( true );

      final DragBankLineWidget widget = new DragBankLineWidget( m_data, m_widget.getMapPanel() );
      m_widget.setDelegate( widget );
    }
    else
    {
      m_data.setBankEdit( false );
      m_widget.setDelegate( null );
    }
    updateData( true );
  }

  public SegmentData getCurrentSegment( )
  {
    return m_data.getSelectedSegment();
  }

  void switchProfile( )
  {
    m_data.switchProfile();
    updateProfilSection();
  }
}