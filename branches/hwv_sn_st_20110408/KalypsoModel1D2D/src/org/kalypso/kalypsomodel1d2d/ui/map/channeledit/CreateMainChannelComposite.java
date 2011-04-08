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
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.kalypso.chart.ui.editor.commandhandler.ExportHandler;
import org.kalypso.chart.ui.editor.commandhandler.MaximizeHandler;
import org.kalypso.chart.ui.editor.mousehandler.DragEditHandler;
import org.kalypso.chart.ui.editor.mousehandler.DragPanHandler;
import org.kalypso.chart.ui.editor.mousehandler.DragZoomInHandler;
import org.kalypso.chart.ui.editor.mousehandler.DragZoomOutHandler;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.swt.custom.ScrolledCompositeCreator;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DUIImages;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.PROF;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.action.ProfilChartActionsEnum;
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

import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.model.layer.ILayerManager;
import de.openali.odysseus.chart.framework.util.ChartUtilities;
import de.openali.odysseus.chart.framework.view.IChartComposite;

/**
 * @author Thomas Jung
 */
public class CreateMainChannelComposite extends Composite
{
  final CreateChannelData m_data;

  final CreateMainChannelWidget m_widget;

  private Section m_profilSection;

  private Section m_segmentSection;

  boolean m_ButtonStateZoom;

  /*********************************************************************************************************************
   * m_buttonList<BR>
   * Following buttons are present in the Schlauchgenerator: <BR>
   * Profile w‰hlen; Uferlinie 1 w‰hlen; Uferlinie 1 zeichnen; Uferlinie 2 w‰hlen; Uferlinie 2 zeichnen:
   */
  private final List<Button> m_buttonList = new ArrayList<Button>();

  private Button m_buttonConvertToModel;

  private final FormToolkit m_toolkit;

  Button m_buttonEditBank;

  private boolean m_bankEdit;

  public boolean isBankEdit( )
  {
    return m_bankEdit;
  }

  public CreateMainChannelComposite( final Composite parent, final FormToolkit toolkit, final int style, final CreateChannelData data, final CreateMainChannelWidget widget )
  {
    super( parent, style );

    data.setShell( getShell() );

    m_toolkit = toolkit;
    m_toolkit.adapt( this );

    m_data = data;
    m_widget = widget;

    try
    {
      init();
    }
    catch( final Throwable e )
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
    final IKalypsoFeatureTheme[] bankThemes = KalypsoFeatureThemeHelper.getLineThemes( m_widget.getPanel() );
    /* Create gui */
    final GridLayout myLayout = new GridLayout( 1, false );
    myLayout.marginWidth = 0;
    myLayout.marginHeight = 0;
    setLayout( myLayout );

    final ScrolledCompositeCreator creator = new ScrolledCompositeCreator( null )
    {
      @SuppressWarnings("synthetic-access")
      @Override
      protected Control createContents( final Composite parent, final int style )
      {
        final Composite contentCompo = m_toolkit.createComposite( parent, style );
        final GridLayout layout = myLayout;
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        contentCompo.setLayout( layout );

        /* Create Profile control */
        final Control profileSection = createProfileSelectionSection( contentCompo, profileThemes );
        final GridData gridDataProf = new GridData( SWT.FILL, SWT.CENTER, true, false );
        profileSection.setLayoutData( gridDataProf );

        /* Create Bank control */
        final Control bankSection = createBankSelectionSection( contentCompo, bankThemes );
        final GridData gridDataBank = new GridData( SWT.FILL, SWT.CENTER, true, false );
        bankSection.setLayoutData( gridDataBank );

        /* Create segment switch control */
        final Control segmentSwitchSection = createSegmentSwitchSection( contentCompo );
        final GridData gridDataSegmentSwitch = new GridData( SWT.FILL, SWT.CENTER, true, false );
        segmentSwitchSection.setLayoutData( gridDataSegmentSwitch );

        /* Create profile control */
        final Control profilSection = createProfilSection( contentCompo );
        final GridData gridDataProfileControl = new GridData( SWT.FILL, SWT.FILL, true, true );
        profilSection.setLayoutData( gridDataProfileControl );

        /* conversion to model composite */
        final Composite compConversion = m_toolkit.createComposite( contentCompo, SWT.NONE );
        compConversion.setLayout( new GridLayout( 2, false ) );

        m_buttonConvertToModel = m_toolkit.createButton( compConversion, "", SWT.PUSH ); //$NON-NLS-1$
        m_buttonConvertToModel.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.15" ) ); //$NON-NLS-1$

        final PluginImageProvider imageProvider = KalypsoModel1D2DPlugin.getImageProvider();

        m_buttonConvertToModel.setImage( imageProvider.getImage( KalypsoModel1D2DUIImages.IMGKEY.OK ) );
        m_buttonConvertToModel.addSelectionListener( new SelectionAdapter()
        {
          @Override
          public void widgetSelected( final SelectionEvent e )
          {
            handleConvertButtonPressed();
          }
        } );

        m_toolkit.createLabel( compConversion, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.42" ), SWT.NULL ); //$NON-NLS-1$

        return contentCompo;
      }
    };
    creator.createControl( this, SWT.V_SCROLL, SWT.NONE );

    creator.getScrolledComposite().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    m_toolkit.adapt( creator.getScrolledComposite() );

    updateControl( true );

    if( m_bankEdit == true )
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

    return m_profilSection;
  }

  /**
   * in the segment section the crosssections will be displayed. the data filling is done in the function
   * "updateSegmentSection" (see below)
   */
  private Control createSegmentSwitchSection( final Composite parent )
  {
    m_segmentSection = m_toolkit.createSection( parent, Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );

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
    sectionClient.setLayout( new GridLayout( 4, false ) );

    m_segmentSection.setClient( sectionClient );
    m_segmentSection.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.0" ) ); //$NON-NLS-1$
    if( m_data.getNumOfSegments() > 0 )
      m_segmentSection.setExpanded( true );
    else
      m_segmentSection.setExpanded( false );

    /** ************************ Header ***************************** */
    /* label */

    final Label labelSpinnnerSegment = m_toolkit.createLabel( sectionClient, "", SWT.NULL ); //$NON-NLS-1$
    labelSpinnnerSegment.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.1" ) ); //$NON-NLS-1$
    final GridData gridDataLabelSpinner = new GridData( SWT.FILL, SWT.CENTER, true, false );
    gridDataLabelSpinner.horizontalSpan = 1;
    labelSpinnnerSegment.setLayoutData( gridDataLabelSpinner );

    /* spinner for specify the current segment */
    final Spinner spinnerSegment = new Spinner( sectionClient, 0 );
    m_toolkit.adapt( spinnerSegment );
    final GridData gridDataSegmentSpinner = new GridData( SWT.END, SWT.CENTER, true, false );
    gridDataSegmentSpinner.horizontalSpan = 3;
    spinnerSegment.setLayoutData( gridDataSegmentSpinner );

    /* zoom to extend button */
    final Button buttonZoomToExtend = new Button( sectionClient, SWT.CHECK );
    buttonZoomToExtend.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.2" ) ); //$NON-NLS-1$
    buttonZoomToExtend.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.43" ) ); //$NON-NLS-1$
    buttonZoomToExtend.setSelection( m_ButtonStateZoom );
    buttonZoomToExtend.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        if( m_ButtonStateZoom == false )
        {
          m_ButtonStateZoom = true;

          final GM_Envelope mapExtend = m_data.getSelectedSegment().getSegmentMapExtend();
          if( mapExtend != null )
          {
            final IMapPanel panel = m_widget.getPanel();
            panel.setBoundingBox( mapExtend );
          }
        }
        else
          m_ButtonStateZoom = false;
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
            final IMapPanel panel = m_widget.getPanel();
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
    groupSegment.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.3" ) ); //$NON-NLS-1$

    final GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 4;
    groupSegment.setLayout( gridLayout );
    final GridData gridData = new GridData( SWT.FILL, SWT.CENTER, true, false );
    gridData.horizontalSpan = 4;
    groupSegment.setLayoutData( gridData );

    final Composite compSegmentDataHeader = new Composite( groupSegment, SWT.NULL );
    final GridLayout gridLayoutDataHeader = new GridLayout();
    gridLayoutDataHeader.numColumns = 2;
    compSegmentDataHeader.setLayout( gridLayoutDataHeader );
    final GridData gridDataHeader = new GridData( SWT.FILL, SWT.CENTER, true, false );
    gridDataHeader.horizontalSpan = 4;
    compSegmentDataHeader.setLayoutData( gridDataHeader );

    final Label labelNumIntersSegment = new Label( compSegmentDataHeader, SWT.NULL );
    labelNumIntersSegment.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.4" ) ); //$NON-NLS-1$
    final GridData gridDatalabel = new GridData( SWT.FILL, SWT.CENTER, true, false );
    gridDatalabel.horizontalSpan = 1;
    labelNumIntersSegment.setLayoutData( gridDatalabel );

    /* spinner for specifiying the number of intersection points for the current segment */
    final Spinner spinnerNumIntersSegment = new Spinner( compSegmentDataHeader, 0 );
    m_toolkit.adapt( spinnerNumIntersSegment );

    final GridData gridDataNumIntersSpinner = new GridData( SWT.RIGHT, SWT.CENTER, true, false );
    gridDataNumIntersSpinner.horizontalSpan = 1;
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

        m_widget.getPanel().repaintMap();
      }
    } );

    final Composite compSegmBanks1 = new Composite( groupSegment, SWT.NULL );
    final GridLayout gridLayoutSegmBanks = new GridLayout();
    gridLayoutSegmBanks.numColumns = 2;
    compSegmBanks1.setLayout( gridLayoutSegmBanks );
    final GridData gridDataSegmBanks = new GridData( SWT.FILL, SWT.CENTER, true, false );
    gridDataSegmBanks.horizontalSpan = 1;
    compSegmBanks1.setLayoutData( gridDataSegmBanks );

    final Label labelBankline = new Label( compSegmBanks1, SWT.NULL );
    labelBankline.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.8" ) ); //$NON-NLS-1$

    /* edit button for bankline 1 */
    m_buttonEditBank = m_toolkit.createButton( compSegmBanks1, "", SWT.TOGGLE ); //$NON-NLS-1$
    m_buttonEditBank.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.9" ) ); //$NON-NLS-1$

    final PluginImageProvider imageProvider = KalypsoModel1D2DPlugin.getImageProvider();
    final Image editImage = imageProvider.getImage( KalypsoModel1D2DUIImages.IMGKEY.EDIT );
    m_buttonEditBank.setImage( editImage );
    m_buttonEditBank.setSelection( m_bankEdit );
    m_buttonEditBank.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        editButtonUpdateBank();
        resetButtonGuard();
        m_widget.getPanel().repaintMap();
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
    combviewerBank1.getControl().setLayoutData( new GridData( SWT.LEFT, SWT.CENTER, true, false ) );
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
    chooseFirstBankButton.setLayoutData( new GridData( SWT.RIGHT, SWT.CENTER, true, false ) );
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
    
    GridData layoutDataFirstButton = new GridData( SWT.RIGHT, SWT.CENTER, true, false );
    layoutDataFirstButton.minimumWidth = 25;
    drawFirstBankButton.setVisible( true );
    drawFirstBankButton.setLayoutData( layoutDataFirstButton );
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
    combviewerBank2.getControl().setLayoutData( new GridData( SWT.LEFT, SWT.CENTER, true, false ) );
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
    final Button chooseSecondBankButton = m_toolkit.createButton( sectionClient, null, SWT.TOGGLE );
    chooseSecondBankButton.setLayoutData( new GridData( SWT.RIGHT, SWT.CENTER, true, false ) );
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

    drawSecondBankButton.setVisible( true );
    drawSecondBankButton.setLayoutData( new GridData( SWT.RIGHT, SWT.CENTER, true, false ) );
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

    final Label bankLabel = new Label( sectionClient, SWT.NULL );
    bankLabel.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.28" ) ); //$NON-NLS-1$
    final GridData gridData = new GridData( SWT.FILL, SWT.CENTER, true, false );
    // gridData.horizontalSpan = 1;
    bankLabel.setLayoutData( gridData );

    final Spinner spinNumBankIntersections = new Spinner( sectionClient, SWT.NONE );

    final GridData gridDataSpin = new GridData( SWT.RIGHT, SWT.CENTER, true, false );
    // gridDataSpin.horizontalSpan = 1;

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
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
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

    final GridData gridDataSpinner = new GridData();
    gridDataSpinner.horizontalAlignment = SWT.RIGHT;
    spinNumBankIntersections.setLayoutData( gridDataSpinner );
    
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
    combviewerProfiles.getControl().setLayoutData( new GridData( SWT.LEFT, SWT.CENTER, true, false ) );
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
    final Button chooseProfilesButton = m_toolkit.createButton( sectionClient, "", SWT.TOGGLE ); //$NON-NLS-1$
    m_buttonList.add( chooseProfilesButton );
    GridData layoutDataFirstButton = new GridData( SWT.RIGHT, SWT.CENTER, true, false );
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
    final GridData gridDataLabel = new GridData( SWT.FILL, SWT.CENTER, true, false );

    /* spinner for specifying the global number of profile intersection points */
    final Label spinnerLabel = new Label( sectionClient, SWT.NULL );
    spinnerLabel.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.36" ) ); //$NON-NLS-1$
    spinnerLabel.setLayoutData( gridDataLabel );

    final Spinner spinNumProfIntersections = new Spinner( sectionClient, SWT.NONE );
    m_toolkit.adapt( spinNumProfIntersections );
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
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
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

    final GridData gridDataSpinner = new GridData();
    gridDataSpinner.horizontalAlignment = SWT.RIGHT;
    spinNumProfIntersections.setLayoutData( gridDataSpinner );
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
   * updates the data for a specific segments
   */
  // private void updateSegmentData( final boolean edit, final SegmentData segment )
  // {
  // m_data.updateSegment( edit, segment );
  // }
  /**
   * displays the selected cross sections -> data filling for the profil section
   */
  private void updateProfilSection( )
  {
    final Control client = m_profilSection.getClient();
    if( client != null && !client.isDisposed() )
      client.dispose();

    final Composite sectionClient = m_toolkit.createComposite( m_profilSection, SWT.NONE );
    final GridLayout clientLayout = new GridLayout( 1, false );
    clientLayout.marginHeight = 0;
    clientLayout.marginWidth = 0;
    sectionClient.setLayout( clientLayout );

    m_profilSection.setClient( sectionClient );
    m_profilSection.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.39" ) ); //$NON-NLS-1$
    m_profilSection.setDescription( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.40" ) ); //$NON-NLS-1$

    m_widget.getPanel().repaintMap();

    final IProfil profil = m_data.getProfil();
    if( profil == null )
    {
      final Label label = m_toolkit.createLabel( sectionClient, "", SWT.NONE ); //$NON-NLS-1$
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

      final ToolBarManager manager = new ToolBarManager( SWT.HORIZONTAL );
      manager.createControl( sectionClient );

      final Label label = m_toolkit.createLabel( sectionClient, "", SWT.BORDER | SWT.CENTER ); //$NON-NLS-1$
      label.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
      label.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.11", profil.getStation() ) ); //$NON-NLS-1$
      label.setBackground( label.getDisplay().getSystemColor( SWT.COLOR_WHITE ) );

      final Control profilControl = profilChartView.createControl( sectionClient );

      profilControl.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

      final ILayerManager mngr = profilChartView.getChart().getChartModel().getLayerManager();
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
          // ((ProfilOverlayLayer) overlayLayer).lockLayer( false );
          m_widget.getPanel().repaintMap();
        }
      }
      final int zOrder = mngr.getLayerPosition( overlayLayer );
      final int last = mngr.getLayers().length - 1;
      if( zOrder < last )
        mngr.moveLayerToPosition( overlayLayer, last );

      final IChartComposite chartComposite = profilChartView.getChartComposite();
      manager.add( ProfilChartActionsEnum.createAction( profilChartView, ProfilChartActionsEnum.ZOOM_OUT, new DragZoomOutHandler( chartComposite ) ) );
      manager.add( ProfilChartActionsEnum.createAction( profilChartView, ProfilChartActionsEnum.ZOOM_IN, new DragZoomInHandler( chartComposite ) ) );
      manager.add( ProfilChartActionsEnum.createAction( profilChartView, ProfilChartActionsEnum.PAN, new DragPanHandler( chartComposite ) ) );
      manager.add( ProfilChartActionsEnum.createAction( profilChartView, ProfilChartActionsEnum.EDIT, new DragEditHandler( chartComposite ) ) );
      manager.add( ProfilChartActionsEnum.createAction( profilChartView, ProfilChartActionsEnum.MAXIMIZE, new MaximizeHandler() ) );
      manager.add( ProfilChartActionsEnum.createAction( profilChartView, ProfilChartActionsEnum.EXPORT_IMAGE, new ExportHandler() ) );
      ChartUtilities.maximize( profilChartView.getChart().getChartModel() );

      final IAction action = new Action( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.44" ), IAction.AS_PUSH_BUTTON ) //$NON-NLS-1$
      {
        /**
         * @see org.eclipse.jface.action.Action#run()
         */
        @SuppressWarnings("synthetic-access")
        @Override
        public void run( )
        {
          // Execute code
          m_data.switchProfile();
          updateProfilSection();
        }
      };
      action.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.14" ) ); //$NON-NLS-1$

      final PluginImageProvider imageProvider = KalypsoModel1D2DPlugin.getImageProvider();
      final ImageDescriptor changeImage = imageProvider.getImageDescriptor( KalypsoModel1D2DUIImages.IMGKEY.CHANGE );
      action.setImageDescriptor( changeImage );
      manager.add( action );

      manager.update( true );
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
      m_bankEdit = true;

      final DragBankLineWidget widget = new DragBankLineWidget( m_data, m_widget.getPanel() );
      m_widget.setDelegate( widget );
    }
    else
    {
      m_bankEdit = false;
      m_widget.setDelegate( null );
    }
    updateData( true );
  }

  protected void handleConvertButtonPressed( )
  {
    final ICoreRunnableWithProgress operation = new ICoreRunnableWithProgress()
    {
      @Override
      public IStatus execute( final IProgressMonitor monitor )
      {
        m_widget.setDelegate( null );
        m_data.convertToModel();
        return Status.OK_STATUS;
      }
    };
    final IStatus status = ProgressUtilities.busyCursorWhile( operation, null );
    ErrorDialog.openError( getShell(), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.46" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.45" ), status ); //$NON-NLS-1$ //$NON-NLS-2$
    if( status.isOK() == true )
    {
      m_data.resetSelectedProfiles();
      resetButtonGuard();
    }
  }

  public SegmentData getCurrentSegment( )
  {
    return m_data.getSelectedSegment();
  }
}
