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
package org.kalypso.model.flood.util;

import java.util.LinkedList;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.ListSelectionDialog;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gml.ui.map.CoverageManagementHelper;
import org.kalypso.gml.ui.map.CoverageThemeInfo;
import org.kalypso.model.flood.binding.IFloodModel;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypso.model.flood.i18n.Messages;
import org.kalypso.ogc.gml.CascadingThemeHelper;
import org.kalypso.ogc.gml.IKalypsoCascadingTheme;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.command.DeleteFeatureCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.template.types.StyledLayerType;
import org.kalypso.template.types.StyledLayerType.Property;
import org.kalypso.template.types.StyledLayerType.Style;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

/**
 * @author Thomas Jung
 */
public class FloodModelHelper
{
  /**
   * gets the index of a given wsp theme inside the cascading "wasserspiegellagen" theme.
   * 
   * @return index of the wsp theme or -1 if none is found
   */
  public static int findWspTheme( final IRunoffEvent runoffEvent, final IKalypsoCascadingTheme wspTheme )
  {
    final IKalypsoTheme[] themes = wspTheme.getAllThemes();

    for( int i = 0; i < themes.length; i++ )
    {
      final IKalypsoTheme theme = themes[i];
      if( theme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme ft = (IKalypsoFeatureTheme) theme;
        final FeatureList featureList = ft.getFeatureList();
        if( featureList != null && featureList.getParentFeatureTypeProperty().getQName().equals( IRunoffEvent.QNAME_PROP_TIN_MEMBER ) )
        {
          final Feature parentFeature = featureList.getParentFeature();
          if( parentFeature.getId().equals( runoffEvent.getFeature().getId() ) )
            return i;
        }
      }
    }
    return -1;
  }

  /**
   * gets the index of a given result theme inside the cascading "wasserspiegellagen" theme.
   * 
   * @return index of the result theme or -1 if none is found
   */
  public static int findResultTheme( final IRunoffEvent runoffEvent, final IKalypsoCascadingTheme wspTheme )
  {
    final IKalypsoTheme[] themes = wspTheme.getAllThemes();

    for( int i = 0; i < themes.length; i++ )
    {
      final IKalypsoTheme theme = themes[i];
      if( theme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme ft = (IKalypsoFeatureTheme) theme;
        final FeatureList featureList = ft.getFeatureList();
        if( featureList != null && featureList.getParentFeature() != null )
        {
          final Feature grandPa = featureList.getParentFeature();
          if( grandPa != null && grandPa.getParentRelation() != null && grandPa.getParentRelation().getQName().equals( IRunoffEvent.QNAME_PROP_RESULT_COVERAGES ) )
          {
            final Feature grandGrandPa = grandPa.getOwner();
            if( grandGrandPa.getId().equals( runoffEvent.getFeature().getId() ) )
              return i;
          }
        }
      }
    }
    return -1;
  }

  /**
   * removes wsp themes in the cascading "wasserspiegellagen" theme; only coverages referenced by the entries of
   * eventsToRemove array will be removed; if eventsToRemove is null, all wsp themes will be removed
   */
  public static void removeWspThemes( final IKalypsoCascadingTheme wspTheme, final IRunoffEvent[] eventsToRemove )
  {
    final IKalypsoTheme[] themes = wspTheme.getAllThemes();

    for( final IKalypsoTheme theme : themes )
    {
      if( theme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme ft = (IKalypsoFeatureTheme) theme;
        final FeatureList featureList = ft.getFeatureList();
        if( featureList == null )
          continue;
        final QName memberFT = featureList.getParentFeatureTypeProperty().getQName();
        if( eventsToRemove != null )
        {
          for( final IRunoffEvent runoffEvent : eventsToRemove )
          {
            Feature parentFeature = featureList.getParentFeature();
            while( parentFeature != null )
            {
              if( memberFT.equals( ICoverageCollection.QNAME_PROP_COVERAGE_MEMBER ) && runoffEvent.getGmlID().equals( parentFeature.getId() ) )
              {
                wspTheme.removeTheme( theme );
                break;
              }
              parentFeature = parentFeature.getOwner();
            }
          }
        }
        else
        {
          // TODO: this is dangerous, because it is possible that other features have property with the same name!
          if( memberFT.equals( ICoverageCollection.QNAME_PROP_COVERAGE_MEMBER ) )
          {
            wspTheme.removeTheme( theme );
          }
        }
      }
    }
  }

  /**
   * adds a coverage theme inside the cascading "wasserspiegellagen" theme for a given event at a given index
   */
  public static void addResultTheme( final IRunoffEvent event, final IKalypsoCascadingTheme theme, final int index ) throws Exception
  {
    final StyledLayerType wspLayer = new StyledLayerType();

    wspLayer.setName( Messages.getString( "org.kalypso.model.flood.util.FloodModelHelper.0", event.getName() ) ); //$NON-NLS-1$
    wspLayer.setFeaturePath( "#fid#" + event.getFeature().getId() + "/" + IRunoffEvent.QNAME_PROP_RESULT_COVERAGES.getLocalPart() + "/" + ICoverageCollection.QNAME_PROP_COVERAGE_MEMBER.getLocalPart() ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    wspLayer.setLinktype( "gml" ); //$NON-NLS-1$
    wspLayer.setType( "simple" ); //$NON-NLS-1$
    wspLayer.setVisible( true );
    wspLayer.setActuate( "onRequest" ); //$NON-NLS-1$
    wspLayer.setHref( "../models/flood.gml" ); //$NON-NLS-1$
    final Property layerPropertyDeletable = new Property();
    layerPropertyDeletable.setName( IKalypsoTheme.PROPERTY_DELETEABLE );
    layerPropertyDeletable.setValue( "false" ); //$NON-NLS-1$

    final Property layerPropertyThemeInfoId = new Property();
    layerPropertyThemeInfoId.setName( IKalypsoTheme.PROPERTY_THEME_INFO_ID );

    final String infoFormat = String.format( Messages.getString( "org.kalypso.model.flood.util.FloodModelHelper.10" ), event.getName() ); //$NON-NLS-1$
    final String infoValue = String.format( "%s?format=%s", CoverageThemeInfo.class.getName(), infoFormat );//$NON-NLS-1$ 
    layerPropertyThemeInfoId.setValue( infoValue );

    final List<Property> layerPropertyList = wspLayer.getProperty();
    layerPropertyList.add( layerPropertyDeletable );
    layerPropertyList.add( layerPropertyThemeInfoId );

    final List<Style> styleList = wspLayer.getStyle();
    final Style style = new Style();
    style.setLinktype( "sld" ); //$NON-NLS-1$
    style.setStyle( "waterdepthUserStyle" ); //$NON-NLS-1$
    style.setActuate( "onRequest" ); //$NON-NLS-1$
    style.setHref( "../maps/results.sld" ); //$NON-NLS-1$
    style.setType( "simple" ); //$NON-NLS-1$
    styleList.add( style );

    if( index == -1 )
      theme.addLayer( wspLayer );
    else
      theme.insertLayer( wspLayer, index );
  }

  /**
   * Removes the specified coverage file.<br/>
   * The model is NOT automatically saved after this operation.
   */
  public static IStatus removeResultCoverages( final SzenarioDataProvider dataProvider, final ICoverageCollection resultCoverages )
  {
    IFeatureBindingCollection<ICoverage> resultCoveragesList = resultCoverages.getCoverages();
    final ICoverage[] coverages = resultCoveragesList.toArray( new ICoverage[resultCoveragesList.size()] );
    try
    {
      final CommandableWorkspace workspace = dataProvider.getCommandableWorkSpace( IFloodModel.class.getName() );

      for( final ICoverage coverageToDelete : coverages )
      {
        /* Delete underlying grid grid file */
        final IStatus status = CoverageManagementHelper.deleteGridFile( coverageToDelete );
        if( !status.isOK() )
          return status;

        /* Delete coverage from collection */
        final Feature coverageFeature = coverageToDelete;

        final DeleteFeatureCommand command = new DeleteFeatureCommand( coverageFeature );
        workspace.postCommand( command );
      }
      return Status.OK_STATUS;
    }
    catch( final Exception e )
    {
      return StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.model.flood.util.FloodModelHelper.1" ) ); //$NON-NLS-1$
    }
  }

  /**
   * shows a {@link ListSelectionDialog} in which the user can select {@link IRunoffEvent} for further processing
   * 
   * @param shell
   * @param events
   *          the RunoffEvents
   * 
   * @return a array of selected {@link IRunoffEvent}
   */
  public static IRunoffEvent[] askUserForEvents( final Shell shell, final IFeatureWrapperCollection<IRunoffEvent> events )
  {
    final LabelProvider labelProvider = new RunoffEventForProcessingLabelProvider();

    final ListSelectionDialog dialog = new ListSelectionDialog( shell, events, new ArrayContentProvider(), labelProvider, Messages.getString( "org.kalypso.model.flood.util.FloodModelHelper.21" ) ); //$NON-NLS-1$
    dialog.setTitle( Messages.getString( "org.kalypso.model.flood.util.FloodModelHelper.22" ) ); //$NON-NLS-1$

    if( dialog.open() != Window.OK )
      return null;

    final Object[] selectedObjects = dialog.getResult();

    final List<IRunoffEvent> selectedEventList = new LinkedList<IRunoffEvent>();
    for( final Object object : selectedObjects )
    {
      if( object instanceof IRunoffEvent )
      {
        selectedEventList.add( (IRunoffEvent) object );
      }
    }
    return selectedEventList.toArray( new IRunoffEvent[selectedEventList.size()] );
  }

  public static IKalypsoFeatureTheme findThemeForEvent( final IMapModell mapModell, final IRunoffEvent runoffEvent )
  {
    final IKalypsoCascadingTheme wspThemes = CascadingThemeHelper.getNamedCascadingTheme( mapModell, Messages.getString( "org.kalypso.model.flood.util.FloodModelHelper.23" ), "waterlevelThemes" ); //$NON-NLS-1$ //$NON-NLS-2$

    if( runoffEvent == null || wspThemes == null )
      return null;

    final int index = findWspTheme( runoffEvent, wspThemes );

    if( index == -1 )
      return null;

    return (IKalypsoFeatureTheme) wspThemes.getAllThemes()[index];
  }
}
