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
 *  Lesser General public License for more details.
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

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata.ChannelEditProfileData;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata.IProfileData;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata.ISegmentData;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata.UpdateEditDataOperation;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypsodeegree.model.geometry.GM_Curve;

/**
 * @author Gernot Belger
 * @author Thomas Jung
 */
public class CreateChannelData extends AbstractModelObject
{
  static final String STR_NO_PROFILE_THEMES = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.32" ); //$NON-NLS-1$

  static final String STR_NO_BANK_THEMES = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.18" ); //$NON-NLS-1$

  static final String STR_DIALOG_TITLE = "Flussschlauchgenerator";

  static final String PROPERTY_PROFILE_THEME_INPUT = "profileThemeInput"; //$NON-NLS-1$

  static final String PROPERTY_PROFILE_THEME_SELECTED = "selectedProfileTheme"; //$NON-NLS-1$

  static final String PROPERTY_PROFILE_THEME_SELECTION_ENABLED = "profileThemeSelectionEnabled"; //$NON-NLS-1$

  static final String PROPERTY_BANK_THEME_INPUT = "bankThemeInput"; //$NON-NLS-1$

  static final String PROPERTY_BANK_THEME_LEFT = "bankThemeLeft"; //$NON-NLS-1$

  static final String PROPERTY_BANK_THEME_SELECTED_LEFT = "selectedBankThemeLeft"; //$NON-NLS-1$

  static final String PROPERTY_BANK_THEME_RIGHT = "bankThemeRight"; //$NON-NLS-1$

  static final String PROPERTY_BANK_THEME_SELECTED_RIGHT = "selectedBankThemeRight"; //$NON-NLS-1$

  static final String PROPERTY_BANK_THEME_SELECTION_ENABLED = "bankThemeSelectionEnabled"; //$NON-NLS-1$

  public static final String PROPERTY_DELEGATE = "delegate"; //$NON-NLS-1$

  public static final String PROPERTY_SELECT_PROFILE_WIDGET_ENABLED = "selectProfileWidgetEnabled"; //$NON-NLS-1$

  public static final String PROPERTY_PROFILE_AUTO_ZOOM = "profileAutoZoom"; //$NON-NLS-1$

  public static final String PROPERTY_PROFILE_DATA_CHOOSER_INPUT = "profileDataChooserInput"; //$NON-NLS-1$

  public static final String PROPERTY_PROFILE_EDITING_ENABLED = "profileEditingEnabled"; //$NON-NLS-1$

  public static final String PROPERTY_ACTIVE_PROFILE = "activeProfile"; //$NON-NLS-1$

  public static final String PROPERTY_NUM_PROFILE_SEGMENTS = "numberProfileSegments"; //$NON-NLS-1$

  public static final String PROPERTY_NUM_BANK_SEGMENTS = "numberBankSegments"; //$NON-NLS-1$

  public static final String PROPERTY_NUM_BANK_SEGMENTS_ENABLED_DOWN = "numberBankSegmentsEnabledDown"; //$NON-NLS-1$

  public static final String PROPERTY_NUM_BANK_SEGMENTS_ENABLED_UP = "numberBankSegmentsEnabledUp"; //$NON-NLS-1$

  public static final String PROPERTY_NUM_BANK_SEGMENTS_DOWN = "numberBankSegmentsDown"; //$NON-NLS-1$

  public static final String PROPERTY_NUM_BANK_SEGMENTS_UP = "numberBankSegmentsUp"; //$NON-NLS-1$

  /* hack: pseudo for triggering a map repaint: works, because any property change triggers a map repaint */
  static final String PROPERTY_MAP_REPAINT = "pseudoPropertyMapRepaint"; //$NON-NLS-1$

  private static final Object[] EMPTY_PROFILE_SELECTION = new String[] { "<no profiles selected in map>" };

  private final Object[] EMPTY_PROFILE_THEME_INPUT = new Object[] { STR_NO_PROFILE_THEMES };

  private final Object[] EMPTY_BANK_THEME_INPUT = new Object[] { STR_NO_BANK_THEMES };

  public enum SIDE
  {
    LEFT,
    RIGHT;
  }

  private final CreateMainChannelWidget m_widget;

  /* Profile selection data */
  private IKalypsoFeatureTheme m_profileTheme;

  private IKalypsoFeatureTheme[] m_profileThemes = new IKalypsoFeatureTheme[0];

  private ChannelEditProfileData m_editData = new ChannelEditProfileData( new IProfileFeature[0], 6, new HashMap<GM_Curve, SIDE>(), 5 );

  /* bankline selection data */
  private IKalypsoFeatureTheme[] m_bankThemes = new IKalypsoFeatureTheme[0];

  private IKalypsoFeatureTheme m_bankThemeLeft;

  private IKalypsoFeatureTheme m_bankThemeRight;

  private IProfileData m_activeProfile;

  private boolean m_profileAutoZoom = false;

  public CreateChannelData( final CreateMainChannelWidget widget )
  {
    m_widget = widget;
  }

  /** The really selected profile theme, may be null, if not theme can be selected */
  IKalypsoFeatureTheme getProfileTheme( )
  {
    return m_profileTheme;
  }

  /** Selection for the profile theme combom */
  public Object getSelectedProfileTheme( )
  {
    if( m_profileTheme == null )
      return STR_NO_PROFILE_THEMES;

    return m_profileTheme;
  }

  /** Setter of selection for the profile theme combom */
  public void setSelectedProfileTheme( final Object selection )
  {
    final Object oldValue = getSelectedProfileTheme();
    final boolean oldEnablement = getSelectProfileWidgetEnabled();

    if( selection instanceof IKalypsoFeatureTheme )
      m_profileTheme = (IKalypsoFeatureTheme)selection;
    else
      m_profileTheme = null;

    firePropertyChange( PROPERTY_PROFILE_THEME_SELECTED, oldValue, getSelectedProfileTheme() );
    firePropertyChange( PROPERTY_SELECT_PROFILE_WIDGET_ENABLED, oldEnablement, getSelectProfileWidgetEnabled() );

    // TODO: reset dependend data?
  }

  public boolean getSelectProfileWidgetEnabled( )
  {
    return m_profileTheme != null;
  }

  void setProfileThemes( final IKalypsoFeatureTheme[] profileThemes )
  {
    final Object[] oldValue = getProfileThemeInput();
    final boolean oldEnablement = getProfileThemeSelectionEnabled();

    m_profileThemes = profileThemes;

    final Object[] newInput = getProfileThemeInput();

    firePropertyChange( PROPERTY_PROFILE_THEME_INPUT, oldValue, getProfileThemeInput() );
    firePropertyChange( PROPERTY_PROFILE_THEME_SELECTION_ENABLED, oldEnablement, getProfileThemeSelectionEnabled() );

    setSelectedProfileTheme( newInput[0] );
  }

  public Object[] getProfileThemeInput( )
  {
    if( m_profileThemes.length == 0 )
      return EMPTY_PROFILE_THEME_INPUT;

    return m_profileThemes;
  }

  public boolean getProfileThemeSelectionEnabled( )
  {
    return m_profileThemes.length > 0;
  }

  void setBankThemes( final IKalypsoFeatureTheme[] bankThemes )
  {
    final Object[] oldValue = getBankThemeInput();
    final boolean oldEnablement = getBankThemeSelectionEnabled();

    m_bankThemes = bankThemes;

    final Object[] newInput = getBankThemeInput();

    firePropertyChange( PROPERTY_BANK_THEME_INPUT, oldValue, getProfileThemeInput() );
    firePropertyChange( PROPERTY_BANK_THEME_SELECTION_ENABLED, oldEnablement, getProfileThemeSelectionEnabled() );

    setSelectedBankThemeLeft( newInput[0] );
    setSelectedBankThemeRight( newInput[0] );
  }

  public Object[] getBankThemeInput( )
  {
    if( m_bankThemes.length == 0 )
      return EMPTY_BANK_THEME_INPUT;

    return m_bankThemes;
  }

  public boolean getBankThemeSelectionEnabled( )
  {
    return m_bankThemes.length > 0;
  }

  public IKalypsoFeatureTheme getBankThemeLeft( )
  {
    return m_bankThemeLeft;
  }

  public Object getSelectedBankThemeLeft( )
  {
    if( m_bankThemeLeft == null )
      return STR_NO_BANK_THEMES;

    return m_bankThemeLeft;
  }

  public IKalypsoFeatureTheme getBankThemeRight( )
  {
    return m_bankThemeRight;
  }

  public Object getSelectedBankThemeRight( )
  {
    if( m_bankThemeRight == null )
      return STR_NO_BANK_THEMES;

    return m_bankThemeRight;
  }

  public void setSelectedBankThemeLeft( final Object selection )
  {
    final Object oldValue = getSelectedBankThemeLeft();

    if( selection instanceof IKalypsoFeatureTheme )
      m_bankThemeLeft = (IKalypsoFeatureTheme)selection;
    else
      m_bankThemeLeft = null;

    firePropertyChange( PROPERTY_BANK_THEME_SELECTED_LEFT, oldValue, getSelectedBankThemeLeft() );
  }

  public void setSelectedBankThemeRight( final Object selection )
  {
    final Object oldValue = getSelectedBankThemeRight();

    if( selection instanceof IKalypsoFeatureTheme )
      m_bankThemeRight = (IKalypsoFeatureTheme)selection;
    else
      m_bankThemeRight = null;

    firePropertyChange( PROPERTY_BANK_THEME_SELECTED_RIGHT, oldValue, getSelectedBankThemeRight() );
  }

  public ChannelEditProfileData getEditData( )
  {
    return m_editData;
  }

  public GM_Curve getBanklineForSide( final SIDE side )
  {
    return m_editData.getBanklineForSide( side );
  }

  /**
   * sets the bankline for a given side.<BR>
   * Before the new line is set the already existing bankline gets deleted as we allow only one line per side.
   *
   * @param curve
   *          the bankline
   * @param side
   *          the side of the bankline
   */
  public void setBankline( final GM_Curve curve, final SIDE side )
  {
    /*
     * clear all existing banklines of the current side, because we allow only one bankline to be drawn for each side
     */

    final Map<GM_Curve, SIDE> banklines = m_editData.getBanklines();

    final Map<GM_Curve, SIDE> newBanklines = new HashMap<>();
    newBanklines.put( curve, side );

    /* copy other side */
    final Set<Entry<GM_Curve, SIDE>> entrySet = banklines.entrySet();
    for( final Entry<GM_Curve, SIDE> entry : entrySet )
    {
      if( entry.getValue() != side )
        newBanklines.put( entry.getKey(), entry.getValue() );
    }

    final ChannelEditProfileData oldData = m_editData;

    final ChannelEditProfileData newData = new ChannelEditProfileData( oldData.getProfileFeatures(), oldData.getNumberProfileSegments(), newBanklines, oldData.getNumberBanklineSegments() );

    startUpdateEditData( newData );
  }

  public void removeBank( final GM_Curve toRemove )
  {
    final Map<GM_Curve, SIDE> banklines = m_editData.getBanklines();

    final Map<GM_Curve, SIDE> newBanklines = new HashMap<>( banklines );

    /* remove obsolete curve */
    newBanklines.remove( toRemove );

    final ChannelEditProfileData oldData = m_editData;

    final ChannelEditProfileData newData = new ChannelEditProfileData( oldData.getProfileFeatures(), oldData.getNumberProfileSegments(), newBanklines, oldData.getNumberBanklineSegments() );

    startUpdateEditData( newData );
  }

  public IProfileData[] getSelectedProfiles( )
  {
    return m_editData.getProfiles();
  }

  IProfileFeature[] getSelectedProfileFeatures( )
  {
    return m_editData.getProfileFeatures();
  }

  private void setSelectedProfiles( final IProfileFeature[] profiles )
  {
    final ChannelEditProfileData oldData = m_editData;

    final ChannelEditProfileData newData = new ChannelEditProfileData( profiles, oldData.getNumberProfileSegments(), oldData.getBanklines(), oldData.getNumberBanklineSegments() );

    startUpdateEditData( newData );
  }

  public void changeSelectedProfiles( final IProfileFeature[] profileFeaturesToRemove, final IProfileFeature[] profileFeaturesToAdd )
  {
    final IProfileFeature[] features = getSelectedProfileFeatures();

    final Set<IProfileFeature> featureHash = new HashSet<>( Arrays.asList( features ) );

    featureHash.removeAll( Arrays.asList( profileFeaturesToRemove ) );
    featureHash.addAll( Arrays.asList( profileFeaturesToAdd ) );

    final IProfileFeature[] newFeatures = featureHash.toArray( new IProfileFeature[featureHash.size()] );

    setSelectedProfiles( newFeatures );
  }

  public void resetSelectedProfiles( )
  {
    setSelectedProfiles( new IProfileFeature[0] );
  }

  public void setNumberProfileSegments( final int numProfileSegments )
  {
    final ChannelEditProfileData oldData = m_editData;

    final ChannelEditProfileData newData = new ChannelEditProfileData( oldData.getProfileFeatures(), numProfileSegments, oldData.getBanklines(), oldData.getNumberBanklineSegments() );

    startUpdateEditData( newData );
  }

  public int getNumberProfileSegments( )
  {
    return m_editData.getNumberProfileSegments();
  }

  public int getNumberBankSegments( )
  {
    return m_editData.getNumberBanklineSegments();
  }

  public void setNumberBankSegments( final int numberBankSegments )
  {
    final ChannelEditProfileData oldData = m_editData;

    final ChannelEditProfileData newData = new ChannelEditProfileData( oldData.getProfileFeatures(), oldData.getNumberProfileSegments(), oldData.getBanklines(), numberBankSegments );

    startUpdateEditData( newData );
  }

  // /**
  // * returns the {@link GM_Position}s of already intersected banklines for aall {@link SegmentData}s.<BR>
  // * The start and end points of the banklines will be neglected.
  // */
  // public GM_Position[] getAllSegmentPosses( )
  // {
  // final List<GM_Position> posList = new ArrayList<GM_Position>();
  //
  // for( final SegmentData segment : m_segmentList )
  // {
  // final List<GM_Position> segmentPosses = getIntersectedBanklinePossesForSegment( segment );
  //
  // posList.addAll( segmentPosses );
  // }
  // return posList.toArray( new GM_Position[posList.size()] );
  // }

  // /**
  // * returns the {@link GM_Position}s of already intersected banklines for a given {@link SegmentData}.<BR>
  // * The start and end points of the banklines will be neglected.
  // */
  // public List<GM_Position> getIntersectedBanklinePossesForSegment( final SegmentData segment )
  // {
  // final List<GM_Position> positionList = new ArrayList<GM_Position>();
  //
  // final Coordinate[] bankLeftInters = segment.getBankLeftInters().getCoordinates();
  // final Coordinate[] bankRightInters = segment.getBankRightInters().getCoordinates();
  // for( int i = 1; i < bankRightInters.length - 1; i++ )
  // {
  // final GM_Position pos = JTSAdapter.wrap( bankRightInters[i] );
  // m_segmentMap.put( pos, segment );
  // positionList.add( pos );
  // }
  //
  // for( int i = 1; i < bankLeftInters.length - 1; i++ )
  // {
  // final GM_Position pos = JTSAdapter.wrap( bankLeftInters[i] );
  // m_segmentMap.put( pos, segment );
  // positionList.add( pos );
  // }
  //
  // return positionList;
  // }

  // /**
  // * returns the {@link SegmentData} at a given {@link GM_Position}
  // */
  // public SegmentData getSegmentAtPosition( final GM_Position position )
  // {
  // return m_segmentMap.get( position );
  // }

//  /**
//   * returns the two original bankline curves as {@link GM_Position}s
//   */
//  public GM_Position[] getBanklinePoses( final SIDE m_side )
//  {
//    final GM_Curve bank = getBanklineForSide( m_side );
//
//    final List<GM_Position> posList = new ArrayList<>();
//
//    try
//    {
//      if( bank != null )
//      {
//
//        final GM_Position[] leftPositions = bank.getAsLineString().getPositions();
//        for( final GM_Position position : leftPositions )
//          posList.add( position );
//      }
//    }
//    catch( final GM_Exception e )
//    {
//      // TODO Auto-generated catch block
//      e.printStackTrace();
//    }
//
//    return posList.toArray( new GM_Position[posList.size()] );
//  }

  public CommandableWorkspace getDiscretisationWorkspace( )
  {
    final IMapPanel mapPanel = m_widget.getMapPanel();
    final IKalypsoFeatureTheme theme = UtilMap.findEditableTheme( mapPanel, Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
    return theme.getWorkspace();
  }

  public void setDelegate( final IWidget delegate )
  {
    final IWidget oldValue = getDelegate();

    m_widget.setDelegate( delegate );

    firePropertyChange( PROPERTY_DELEGATE, oldValue, delegate );
  }

  public IWidget getDelegate( )
  {
    return m_widget.getDelegate();
  }

  public IProfileData getActiveProfile( )
  {
    return m_activeProfile;
  }

  public void setActiveProfile( final IProfileData activeProfile )
  {
    final IProfileData oldValue = m_activeProfile;

    final int oldNumBankSegmentsDown = getNumberBankSegmentsDown();
    final int oldNumBankSegmentsUp = getNumberBankSegmentsUp();
    final boolean oldNumBankSegmentsEnabledDown = getNumberBankSegmentsEnabledDown();
    final boolean oldNumBankSegmentsEnabeldUp = getNumberBankSegmentsEnabledUp();

    m_activeProfile = activeProfile;

    firePropertyChange( PROPERTY_ACTIVE_PROFILE, oldValue, activeProfile );

    firePropertyChange( PROPERTY_NUM_BANK_SEGMENTS_DOWN, oldNumBankSegmentsDown, getNumberBankSegmentsDown() );
    firePropertyChange( PROPERTY_NUM_BANK_SEGMENTS_UP, oldNumBankSegmentsUp, getNumberBankSegmentsUp() );
    firePropertyChange( PROPERTY_NUM_BANK_SEGMENTS_ENABLED_DOWN, oldNumBankSegmentsEnabledDown, getNumberBankSegmentsEnabledDown() );
    firePropertyChange( PROPERTY_NUM_BANK_SEGMENTS_ENABLED_UP, oldNumBankSegmentsEnabeldUp, getNumberBankSegmentsEnabledUp() );
  }

  public boolean getProfileAutoZoom( )
  {
    return m_profileAutoZoom;
  }

  public void setProfileAutoZoom( final boolean profileAutoZoom )
  {
    final boolean oldValue = m_profileAutoZoom;

    m_profileAutoZoom = profileAutoZoom;

    firePropertyChange( PROPERTY_PROFILE_AUTO_ZOOM, oldValue, profileAutoZoom );
  }

  public Object[] getProfileDataChooserInput( )
  {
    final IProfileData[] profiles = m_editData.getProfiles();
    if( ArrayUtils.isEmpty( profiles ) )
      return EMPTY_PROFILE_SELECTION;

    return profiles;
  }

  public boolean getProfileEditingEnabled( )
  {
    final IProfileData[] profiles = m_editData.getProfiles();
    if( ArrayUtils.isEmpty( profiles ) )
      return false;

    return profiles.length > 0;
  }

  public ISegmentData[] getSegments( )
  {
    final Set<ISegmentData> segments = new LinkedHashSet<>();

    final ChannelEditProfileData editData = m_editData;

    if( editData != null )
    {
      final IProfileData[] profiles = editData.getProfiles();
      for( final IProfileData profile : profiles )
      {
        segments.add( profile.getDownSegment() );
        segments.add( profile.getUpSegment() );
      }
    }

    segments.remove( null );

    return segments.toArray( new ISegmentData[segments.size()] );
  }

  public boolean getNumberBankSegmentsEnabledDown( )
  {
    final ISegmentData segment = getActiveSegmentDown();
    return segment != null && segment.hasBanks();
  }

  public boolean getNumberBankSegmentsEnabledUp( )
  {
    final ISegmentData segment = getActiveSegmentUp();
    return segment != null && segment.hasBanks();
  }

  public void setNumberBankSegmentsDown( final int segments )
  {
    final ISegmentData segment = getActiveSegmentDown();

    updateNumberOfBankSegments( segment, segments, PROPERTY_NUM_BANK_SEGMENTS_DOWN );
  }

  public int getNumberBankSegmentsDown( )
  {
    final ISegmentData segment = getActiveSegmentDown();
    if( segment == null )
      return 0;

    return segment.getNumberBankSegments();
  }

  public void setNumberBankSegmentsUp( final int segments )
  {
    final ISegmentData segment = getActiveSegmentUp();

    updateNumberOfBankSegments( segment, segments, PROPERTY_NUM_BANK_SEGMENTS_UP );
  }

  public int getNumberBankSegmentsUp( )
  {
    final ISegmentData segment = getActiveSegmentUp();
    if( segment == null )
      return 0;

    return segment.getNumberBankSegments();
  }

  private ISegmentData getActiveSegmentDown( )
  {
    if( m_activeProfile == null )
      return null;

    return m_activeProfile.getDownSegment();
  }

  private ISegmentData getActiveSegmentUp( )
  {
    if( m_activeProfile == null )
      return null;

    return m_activeProfile.getUpSegment();
  }

  private void startUpdateEditData( final ChannelEditProfileData newData )
  {
    final ChannelEditProfileData oldData = m_editData;

    final UpdateEditDataOperation operation = new UpdateEditDataOperation( oldData, newData );

    final IProfileData oldActiveProfile = getActiveProfile();
    final IProfileData newActiveProfile = determineNewActiveProfile( newData.getProfiles(), oldActiveProfile );

    setEditData( newData, oldActiveProfile, newActiveProfile );

    final Display display = PlatformUI.getWorkbench().getDisplay();
    display.asyncExec( new Runnable()
    {
      @Override
      public void run( )
      {
        final Shell shell = display.getActiveShell();

        final IStatus status = ProgressUtilities.busyCursorWhile( operation );
        if( !status.isOK() )
          StatusDialog.open( shell, status, STR_DIALOG_TITLE );

        if( operation.hasDataLoss() )
        {
          /* ask user to apply anyways */
          final boolean shouldRollback = askForUserEdits( shell );
          if( shouldRollback )
          {
            setEditData( oldData, newActiveProfile, oldActiveProfile );
            /* trigger map repaint */
            firePropertyChange( PROPERTY_MAP_REPAINT, 0, 1 );
            return;
          }
        }

        /* force some property changed, that are not corretly handled */
        firePropertyChange( PROPERTY_NUM_BANK_SEGMENTS_DOWN, getNumberBankSegmentsDown() + 1, getNumberBankSegmentsDown() );
        firePropertyChange( PROPERTY_NUM_BANK_SEGMENTS_UP, getNumberBankSegmentsUp() + 1, getNumberBankSegmentsUp() );
        firePropertyChange( PROPERTY_NUM_BANK_SEGMENTS_ENABLED_DOWN, !getNumberBankSegmentsEnabledDown(), getNumberBankSegmentsEnabledDown() );
        firePropertyChange( PROPERTY_NUM_BANK_SEGMENTS_ENABLED_UP, !getNumberBankSegmentsEnabledUp(), getNumberBankSegmentsEnabledUp() );
      }
    } );
  }

  protected boolean askForUserEdits( final Shell shell )
  {
    return !MessageDialog.openQuestion( shell, STR_DIALOG_TITLE, "Vom Benutzer editierte Daten gehen verloren. Fortfahren?" );
  }

  void setEditData( final ChannelEditProfileData newData, final IProfileData oldActiveProfile, final IProfileData newActiveProfile )
  {
    final int oldNumberBankSegments = getNumberBankSegments();
    final int oldNumberProfileSegments = getNumberProfileSegments();
    final Object[] oldDataChooserInput = getProfileDataChooserInput();
    final boolean oldProfileEditingEnabled = getProfileEditingEnabled();

    final int oldNumBankSegmentsDown = getNumberBankSegmentsDown();
    final int oldNumBankSegmentsUp = getNumberBankSegmentsUp();
    final boolean oldNumBankSegmentsEnabledDown = getNumberBankSegmentsEnabledDown();
    final boolean oldNumBankSegmentsEnabeldUp = getNumberBankSegmentsEnabledUp();

    m_editData = newData;
    m_activeProfile = newActiveProfile;

    firePropertyChange( PROPERTY_NUM_PROFILE_SEGMENTS, oldNumberProfileSegments, getNumberProfileSegments() );
    firePropertyChange( PROPERTY_NUM_BANK_SEGMENTS, oldNumberBankSegments, getNumberBankSegments() );

    firePropertyChange( PROPERTY_PROFILE_DATA_CHOOSER_INPUT, oldDataChooserInput, getProfileDataChooserInput() );
    firePropertyChange( PROPERTY_PROFILE_EDITING_ENABLED, oldProfileEditingEnabled, getProfileEditingEnabled() );
    firePropertyChange( PROPERTY_ACTIVE_PROFILE, oldActiveProfile, newActiveProfile );

    firePropertyChange( PROPERTY_NUM_BANK_SEGMENTS_DOWN, oldNumBankSegmentsDown, getNumberBankSegmentsDown() );
    firePropertyChange( PROPERTY_NUM_BANK_SEGMENTS_UP, oldNumBankSegmentsUp, getNumberBankSegmentsUp() );
    firePropertyChange( PROPERTY_NUM_BANK_SEGMENTS_ENABLED_DOWN, oldNumBankSegmentsEnabledDown, getNumberBankSegmentsEnabledDown() );
    firePropertyChange( PROPERTY_NUM_BANK_SEGMENTS_ENABLED_UP, oldNumBankSegmentsEnabeldUp, getNumberBankSegmentsEnabledUp() );
  }

  /* Find the profile in the given array that wraps the same feature as the current active profile. */
  private static IProfileData determineNewActiveProfile( final IProfileData[] newProfiles, final IProfileData oldActiveProfile )
  {
    final IProfileData firstNewProfile = newProfiles.length > 0 ? newProfiles[0] : null;

    if( oldActiveProfile == null )
      return firstNewProfile;

    /* search profile with same feature */
    final IProfileFeature activeFeature = oldActiveProfile.getFeature();
    for( final IProfileData newProfile : newProfiles )
    {
      if( newProfile.getFeature() == activeFeature )
        return newProfile;
    }

    return firstNewProfile;
  }

  private void updateNumberOfBankSegments( final ISegmentData segment, final int segments, final String propertySegments )
  {
    final ChannelEditProfileData data = m_editData;
    if( data == null )
      return;

    if( segment == null )
      return;

    final int oldNumberOfSegments = segment.getNumberBankSegments();
    if( oldNumberOfSegments == segments )
      return;

    // REMARK: already fire change, so rollback will later be able to reset the ui
    firePropertyChange( propertySegments, oldNumberOfSegments, segments );

    if( segment.isUserChanged() )
    {
      askForUserEdits( PlatformUI.getWorkbench().getDisplay().getActiveShell() );

      firePropertyChange( propertySegments, segments, oldNumberOfSegments );
    }

    segment.updateNumberOfSegments( segments );
    firePropertyChange( propertySegments, oldNumberOfSegments, getNumberBankSegments() );
  }
}