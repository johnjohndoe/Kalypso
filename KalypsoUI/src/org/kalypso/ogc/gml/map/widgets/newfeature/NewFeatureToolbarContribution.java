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
package org.kalypso.ogc.gml.map.widgets.newfeature;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.IParameter;
import org.eclipse.core.commands.common.NotDefinedException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.ISourceProviderListener;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.services.IServiceLocator;
import org.kalypso.contribs.eclipse.ui.actions.CommandContributionItem;
import org.kalypso.contribs.eclipse.ui.actions.DropDownToolbarItem;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanelSourceProvider;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.catalogs.FeatureTypeImageCatalog;
import org.kalypso.ui.editor.actions.FeatureActionUtilities;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * This class provides a special {@link org.kalypso.contribs.eclipse.jface.action.DropdownContributionItem} that
 * contains commands to add new features of all available feature types. The command
 * "org.kalypso.gis.newFeature.command" is called with a different command parameter for each feature type. The command
 * parameter is the QName of the feature type in its String representation.
 * 
 * @author kurzbach
 */
public class NewFeatureToolbarContribution extends DropDownToolbarItem
{

  private Command m_rootCommand;

  private ISourceProviderListener m_sourceProviderListener;

  private IKalypsoTheme m_currentTheme = null;

  private CommandContributionItem[] m_currentItems = new CommandContributionItem[0];

  public NewFeatureToolbarContribution( )
  {
    // create ISourceProviderListener to listen for changes in the active map panel or active theme
    m_sourceProviderListener = new ISourceProviderListener()
    {

      @SuppressWarnings("unchecked")
      public void sourceChanged( int sourcePriority, Map sourceValuesByName )
      {
        updateInternal();
      }

      public void sourceChanged( int sourcePriority, String sourceName, Object sourceValue )
      {
        updateInternal();
      }

    };
    MapPanelSourceProvider.getInstance().addSourceProviderListener( m_sourceProviderListener );

    // register the base command
    m_rootCommand = m_commandService.getCommand( "org.kalypso.gis.newFeature.command" ); //$NON-NLS-1$
    registerCommandListeners( new Command[] { m_rootCommand } );
  }

  /**
   * @see org.eclipse.ui.actions.CompoundContributionItem#getContributionItems()
   */
  @SuppressWarnings("unchecked")
  @Override
  protected CommandContributionItem[] getContributionItems( )
  {
    IParameter parameter = null;
    try
    {
      parameter = m_rootCommand.getParameter( NewFeatureHandler.PARAMETER_FEATURE_TYPE );
    }
    catch( final NotDefinedException e )
    {
      e.printStackTrace();
      return new CommandContributionItem[0];
    }

    final IHandlerService handlerService = (IHandlerService) PlatformUI.getWorkbench().getService( IHandlerService.class );
    final IEvaluationContext currentState = handlerService.getCurrentState();
    final IKalypsoTheme theme = (IKalypsoTheme) currentState.getVariable( MapPanelSourceProvider.ACTIVE_THEME_NAME );


    if( theme == null || !theme.isLoaded() )
    {
      // do not cache themes that are not loaded yet
      return new CommandContributionItem[0];
    }

    // use cached items
    if( theme == m_currentTheme )
    {
      return m_currentItems;
    }

    m_currentTheme = theme;
    
    if( theme == null || !(theme instanceof IKalypsoFeatureTheme) )
    {
      m_currentItems = new CommandContributionItem[0];
      return m_currentItems;
    }

    final IKalypsoFeatureTheme featureTheme = (IKalypsoFeatureTheme) theme;
    final IFeatureType featureType = featureTheme.getFeatureType();
    
    if( featureType == null )
    {
      m_currentItems = new CommandContributionItem[0];
      return m_currentItems;
    }
    
    final FeatureList featureList = featureTheme.getFeatureList();
    final Feature parentFeature = featureList.getParentFeature();
    final IRelationType fatp = featureList.getParentFeatureTypeProperty();
    
    final int maxOccurs = fatp.getMaxOccurs();

    /* If we may not inline features we cannot create them via 'new' */
    if( !fatp.isInlineAble() )
      // Just return
      return null;

    /*
     * Direct properties (maxoccurs = 1) can only be added if not already there.
     */
    if( maxOccurs == 1 && parentFeature.getProperty( fatp ) != null )
    {
      m_currentItems = new CommandContributionItem[0];
      return m_currentItems;
    }

    /*
     * If maxoccurs > 1 we have a list, and we may test if the list is already full.
     */
    else if( maxOccurs > 1 )
    {
      final List list = (List) parentFeature.getProperty( fatp );
      if( list != null && list.size() >= maxOccurs )
      {
        m_currentItems = new CommandContributionItem[0];
        return m_currentItems;
      }
    }

    final IGMLSchema contextSchema = featureType.getGMLSchema();
    final IFeatureType[] featureTypes = GMLSchemaUtilities.getSubstituts( featureType, contextSchema, false, true );

    final CommandContributionItem[] items = new CommandContributionItem[featureTypes.length];
    final IServiceLocator locator = PlatformUI.getWorkbench();
    int i = 0;
    for( final IFeatureType ft : featureTypes )
    {
      final String parameterLabel = FeatureActionUtilities.newFeatureActionLabel( ft );
      final ImageDescriptor catalogDescriptor = FeatureTypeImageCatalog.getImage( null, ft.getQName() );
      final ImageDescriptor featureNewImg = catalogDescriptor == null ? ImageProvider.IMAGE_FEATURE_NEW : catalogDescriptor;
      final IAnnotation annotation = featureType.getAnnotation();

      String tooltip = null;
      if( annotation != null && !FeatureHelper.hasReplaceTokens( featureType, IAnnotation.ANNO_TOOLTIP ) )
      {
        tooltip = annotation.getValue( IAnnotation.ANNO_TOOLTIP );
      }

      final Map map = new HashMap();
      final String parameterValue = ft.getQName().toString();
      map.put( parameter.getId(), parameterValue );

      items[i++] = new CommandContributionItem( locator, m_rootCommand.getId(), m_rootCommand.getId(), map, featureNewImg, null, null, parameterLabel, null, tooltip, CommandContributionItem.STYLE_PUSH );
    }
    m_currentItems = items;
    return m_currentItems;
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.actions.DropDownToolbarItem#dispose()
   */
  @Override
  public void dispose( )
  {
    MapPanelSourceProvider.getInstance().removeSourceProviderListener( m_sourceProviderListener );
    m_sourceProviderListener = null;
    super.dispose();
    m_rootCommand = null;
    m_currentTheme = null;
    m_currentItems = null;
  }

  protected void updateInternal( )
  {
    final CommandContributionItem[] contributionItems = getContributionItems();
    if( contributionItems.length > 0)
      m_currentCommand = contributionItems[0];
    final Display display = PlatformUI.getWorkbench().getDisplay();
    display.asyncExec( new Runnable()
    {
      @SuppressWarnings("synthetic-access")
      public void run( )
      {
        NewFeatureToolbarContribution.super.update();
      }
    } );
  }
}
