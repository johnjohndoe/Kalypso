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
package org.kalypso.model.flood.util;

import java.util.LinkedList;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.ListSelectionDialog;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gml.ui.map.CoverageManagmentHelper;
import org.kalypso.gml.ui.map.CoverageThemeInfo;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.flood.binding.IFloodModel;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypso.ogc.gml.AbstractCascadingLayerTheme;
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
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

/**
 * @author Thomas Jung
 * 
 */
public class FloodModelHelper
{

  /**
   * gets the index of a given wsp theme inside the cascading "wasserspiegellagen" theme.
   * 
   * @return index of the wsp theme or -1 if none is found
   */
  public static int findWspTheme( IRunoffEvent runoffEvent, AbstractCascadingLayerTheme wspTheme )
  {
    final IKalypsoTheme[] themes = wspTheme.getAllThemes();

    for( int i = 0; i < themes.length; i++ )
    {
      final IKalypsoTheme theme = themes[i];
      if( theme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme ft = (IKalypsoFeatureTheme) theme;
        final FeatureList featureList = ft.getFeatureList();
        if( featureList.getParentFeatureTypeProperty().getQName().equals( IRunoffEvent.QNAME_PROP_TIN_MEMBER ) )
        {
          final Feature parentFeature = featureList.getParentFeature();
          if( parentFeature.getId().equals( runoffEvent.getWrappedFeature().getId() ) )
            return i;
        }
      }
    }
    return -1;
  }

  /**
   * removes all wsp themes in the cascading "wasserspiegellagen" theme
   */
  public static void removeWspTheme( final AbstractCascadingLayerTheme wspTheme )
  {
    final IKalypsoTheme[] themes = wspTheme.getAllThemes();

    for( IKalypsoTheme theme : themes )
    {
      if( theme instanceof IKalypsoFeatureTheme )
      {
        IKalypsoFeatureTheme ft = (IKalypsoFeatureTheme) theme;
        final FeatureList featureList = ft.getFeatureList();
        final QName name = featureList.getParentFeatureTypeProperty().getQName();
        if( name.equals( ICoverageCollection.QNAME_PROP_COVERAGE_MEMBER ) )
        {
          wspTheme.removeTheme( theme );
        }
      }
    }
  }

  /**
   * adds a coverage theme inside the cascading "wasserspiegellagen" theme for a given event at a given index
   */
  public static void addResultTheme( IRunoffEvent event, AbstractCascadingLayerTheme theme, int index ) throws Exception
  {
    final StyledLayerType wspLayer = new StyledLayerType();

    wspLayer.setName( "Fliesstiefen (" + event.getName() + ")" );
    wspLayer.setFeaturePath( "#fid#" + event.getWrappedFeature().getId() + "/" + IRunoffEvent.QNAME_PROP_RESULT_COVERAGES.getLocalPart() + "/"
        + ICoverageCollection.QNAME_PROP_COVERAGE_MEMBER.getLocalPart() );
    wspLayer.setLinktype( "gml" );
    wspLayer.setType( "simple" );
    wspLayer.setVisible( true );
    wspLayer.setActuate( "onRequest" );
    wspLayer.setHref( "../models/flood.gml" );
    final Property layerPropertyDeletable = new Property();
    layerPropertyDeletable.setName( IKalypsoTheme.PROPERTY_DELETEABLE );
    layerPropertyDeletable.setValue( "false" );

    final Property layerPropertyThemeInfoId = new Property();
    layerPropertyThemeInfoId.setName( IKalypsoTheme.PROPERTY_THEME_INFO_ID );
    layerPropertyThemeInfoId.setValue( CoverageThemeInfo.class.getName() + "?format=Fliesstiefen (" + event.getName() + ") %.2f NN+m" );

    final List<Property> layerPropertyList = wspLayer.getProperty();
    layerPropertyList.add( layerPropertyDeletable );
    layerPropertyList.add( layerPropertyThemeInfoId );

    final List<Style> styleList = wspLayer.getStyle();
    final Style style = new Style();
    style.setLinktype( "sld" );
    style.setStyle( "waterdepthUserStyle" );
    style.setActuate( "onRequest" );
    style.setHref( "../maps/results.sld" );
    style.setType( "simple" );
    styleList.add( style );

    if( index == -1 )
      theme.addLayer( wspLayer );
    else
      theme.insertLayer( wspLayer, index );
  }

  /**
   * removes the specified coverage file
   */
  public static IStatus removeResultCoverages( final Shell shell, final SzenarioDataProvider dataProvider, final ICoverageCollection resultCoverages )
  {
    final ICoverage[] coverages = resultCoverages.toArray( new ICoverage[resultCoverages.size()] );
    try
    {
      final CommandableWorkspace workspace = dataProvider.getCommandableWorkSpace( IFloodModel.class );

      for( ICoverage coverageToDelete : coverages )
      {
        /* Delete underlying grid grid file */
        final IStatus status = CoverageManagmentHelper.deleteGridFile( coverageToDelete );
        ErrorDialog.openError( shell, "Löschen von Raster-Daten fehlgeschlagen", "Rasterdatei (" + coverageToDelete.getName() + ") konnte nicht gelöscht werden.", status );

        if( status == Status.OK_STATUS )
        {
          /* Delete coverage from collection */
          final Feature parentFeature = resultCoverages.getWrappedFeature();
          final IRelationType pt = (IRelationType) parentFeature.getFeatureType().getProperty( ICoverageCollection.QNAME_PROP_COVERAGE_MEMBER );
          final Feature coverageFeature = coverageToDelete.getWrappedFeature();

          final DeleteFeatureCommand command = new DeleteFeatureCommand( workspace, parentFeature, pt, coverageFeature );
          workspace.postCommand( command );

          /* save the model */
          // TODO: use a flag if the model should be getting save
          dataProvider.saveModel( IFloodModel.class, new NullProgressMonitor() );
        }
        return status;
      }
      return Status.OK_STATUS;
    }
    catch( Exception e )
    {
      return StatusUtilities.statusFromThrowable( e, "Löschen von Raster-Daten fehlgeschlagen" );
    }
  }

  /**
   * Finds the wsp cascading theme containing the event themes.
   */
  public static AbstractCascadingLayerTheme findWspTheme( final IMapModell mapModell )
  {
    final IKalypsoTheme[] allThemes = mapModell.getAllThemes();
    for( final IKalypsoTheme kalypsoTheme : allThemes )
    {
      // REMARK: not nice, but not otherwise possible: use name to find the theme.
      if( kalypsoTheme instanceof AbstractCascadingLayerTheme && kalypsoTheme.getName().equals( "Wasserspiegellagen" ) )
        return (AbstractCascadingLayerTheme) kalypsoTheme;
    }

    return null;
  }

  /**
   * shows a {@link ListSelectionDialog} in which the user can select {@link IRunoffEvent} for further processing
   * 
   * @param shell
   * @param events
   *            the RunoffEvents
   * 
   * @return a array of selected {@link IRunoffEvent}
   */
  public static IRunoffEvent[] askUserForEvents( final Shell shell, final IFeatureWrapperCollection<IRunoffEvent> events )
  {
    final LabelProvider labelProvider = new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( Object element )
      {
        final IRunoffEvent event = (IRunoffEvent) element;
        ICoverageCollection resultCoverages = event.getResultCoverages();
        if( resultCoverages.size() > 0 )
        {
          return event.getName() + " (Ergebnisse vorhanden)";
        }
        else
          return event.getName();
      }
    };

    final ListSelectionDialog dialog = new ListSelectionDialog( shell, events, new ArrayContentProvider(), labelProvider, "Welche Ereignisse sollen verarbeitet werden?" );
    dialog.setTitle( "Flood-Modeller" );

    if( dialog.open() != Window.OK )
      return null;

    Object[] selectedObjects = dialog.getResult();

    List<IRunoffEvent> selectedEventList = new LinkedList<IRunoffEvent>();
    for( Object object : selectedObjects )
    {
      if( object instanceof IRunoffEvent )
      {
        selectedEventList.add( (IRunoffEvent) object );
      }
    }
    return selectedEventList.toArray( new IRunoffEvent[selectedEventList.size()] );
  }
}
