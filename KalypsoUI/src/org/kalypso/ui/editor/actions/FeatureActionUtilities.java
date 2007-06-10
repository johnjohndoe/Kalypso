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
package org.kalypso.ui.editor.actions;

import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider.ImageKey;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.annotation.AnnotationUtilities;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.ImageProvider.DESCRIPTORS;
import org.kalypso.ui.catalogs.FeatureTypeImageCatalog;
import org.kalypso.ui.editor.gmleditor.ui.FeatureAssociationTypeElement;
import org.kalypso.ui.editor.gmleditor.ui.NewFeatureAction;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Utility class to help with actions on features.
 * 
 * @author Gernot Belger
 */
public class FeatureActionUtilities
{
  private static final class ListFullAction extends Action
  {
    private final int m_occurs;

    protected ListFullAction( final int occurs )
    {
      super( "Maximale Grösse erreicht" );

      final ImageKey[] overlays = new ImageKey[] { null, DESCRIPTORS.FORBIDDEN_OVR, null, null, null };
      final ImageDescriptor forbiddenImgDesc = KalypsoGisPlugin.getImageProvider().getDecoratedImageDescriptor( DESCRIPTORS.FEATURE, overlays );
      setImageDescriptor( forbiddenImgDesc );

      m_occurs = occurs;
    }

    /**
     * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
     */
    @Override
    public void runWithEvent( final Event event )
    {
      final Shell shell = event.widget.getDisplay().getActiveShell();
      MessageDialog.openInformation( shell, "Maximale Grösse ereicht", "Die maximale Grösse (" + m_occurs
          + ") dieser Liste ist ereicht.\nSie können zu dieser Eigenschaft keine weiteren Elemente hinzufügen ohne vorher Elemente zu löschen." );
    }
  }

  /**
   * Adds a new feature from an yet unknown schema to a workspace. TODO: no yet implemented
   * 
   * @author Gernot Belger
   */
  private static final class NewFeatureFromExternalSchemaAction extends Action
  {
    protected NewFeatureFromExternalSchemaAction( )
    {
      super( "Feature aus zusätzlichem Schema..." );
    }

    /**
     * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
     */
    @Override
    public void runWithEvent( final Event event )
    {
      final Shell shell = event.widget.getDisplay().getActiveShell();
      MessageDialog.openInformation( shell, "Neues Feature aus zusätzlichem Schema", "Diese Operation ist noch nicht implementiert.\nDer Anwender sollte hier zuerst ein GML-Schema auswählen und danach einen passenden Feature-Typ." );
    }
  }

  private FeatureActionUtilities( )
  {
    // will not get instantiated
  }

  /**
   * Create a 'New' submenu with actions to add new features.
   * <p>
   * Works only, if the first element of the selection is of type {@link FeatureAssociationTypeElement}.
   */
  public static IMenuManager createFeatureNewMenu( final IStructuredSelection selection, final IFeatureSelectionManager selectionManager )
  {
    final IMenuManager newMenuManager = new MenuManager( "&Neu" );

    if( selection.size() != 1 )
      return newMenuManager;

    final Object element = selection.getFirstElement();

    final IRelationType fatp;
    final Feature parentFeature;
    final IFeatureType featureType;

    if( element instanceof FeatureAssociationTypeElement )
    {
      final FeatureAssociationTypeElement fate = (FeatureAssociationTypeElement) element;
      parentFeature = fate.getParentFeature();
      fatp = fate.getAssociationTypeProperty();
      featureType = fatp.getTargetFeatureType();
    }
    else if( selection instanceof IFeatureSelection )
    {
      final IFeatureSelection featureSelection = (IFeatureSelection) selection;
      final Feature feature = FeatureSelectionHelper.getFirstFeature( featureSelection );
      // to avoid NullPointerException: the first feature is null ( e.g. a linked feature is selected )
      if( feature == null )
        return newMenuManager;
      fatp = featureSelection.getParentFeatureProperty( feature );
      featureType = feature.getFeatureType();
      parentFeature = feature.getParent();
    }
    else
      return newMenuManager;

    CommandableWorkspace workspace = null;
    if( selection instanceof IFeatureSelection )
      workspace = ((IFeatureSelection) selection).getWorkspace( parentFeature );
    
    if( fatp == null || parentFeature == null || workspace == null )
      return newMenuManager;

    final int maxOccurs = fatp.getMaxOccurs();

    /* If we may not inline features we cannot create them via 'new' */
    if( !fatp.isInlineAble() )
      // Just return, hide menu
      return newMenuManager;
    
    /* Direct properties (maxoccurs = 1) can only be added if not already there. */
    if( maxOccurs == 1 && parentFeature.getProperty( fatp ) != null )
    {
      // Just return, hide menu
      return newMenuManager;
    }
    /* If maxoccurs < 0 we have a list, and we may tet if the list is already full. */
    else if( maxOccurs > 1 )
    {
      final List list = (List) parentFeature.getProperty( fatp );
      if( list != null && list.size() >= maxOccurs )
      {
        // Add an action which indicates, that the list is full
        newMenuManager.add( new ListFullAction( maxOccurs ) );
        return newMenuManager;
      }
    }

    final GMLWorkspace contextWorkspace = parentFeature == null ? null : parentFeature.getWorkspace();
    final IGMLSchema contextSchema = contextWorkspace == null ? null : contextWorkspace.getGMLSchema();
    
    final IFeatureType[] featureTypes = GMLSchemaUtilities.getSubstituts( featureType, contextSchema, false, true );
    for( final IFeatureType ft : featureTypes )
    {
      final String actionLabel = newFeatureActionLabel( ft );

      final ImageDescriptor catalogDescriptor = FeatureTypeImageCatalog.getImage( null, ft.getQName() );

      final ImageDescriptor featureNewImg = catalogDescriptor == null ? ImageProvider.IMAGE_FEATURE_NEW : catalogDescriptor;

      newMenuManager.add( new NewFeatureAction( actionLabel, featureNewImg, workspace, parentFeature, fatp, ft, selectionManager ) );
    }

    newMenuManager.add( new Separator( "additions" ) );
    newMenuManager.add( new NewFeatureFromExternalSchemaAction() );

    return newMenuManager;
  }

  /**
   * Search for a suitable name of the new feature action.
   * <p>
   * The name is searched as follow:
   * </p>
   * <p>
   * First the name-annotations, if no token replace takes place.
   * </p>
   * <p>
   * Second the label-annotations, if no token replace takes place.
   * </p>
   * <p>
   * Last, the local part of the feature type qname.
   * </p>
   */
  private static String newFeatureActionLabel( final IFeatureType featureType )
  {
    final IAnnotation annotation = AnnotationUtilities.getAnnotation( featureType );

    if( annotation != null && !FeatureHelper.hasReplaceTokens( featureType, IAnnotation.ANNO_NAME ) )
    {
      final String name = annotation.getValue( IAnnotation.ANNO_NAME );
      if( name != null )
        return name;
    }

    if( annotation != null && !FeatureHelper.hasReplaceTokens( featureType, IAnnotation.ANNO_LABEL ) )
    {
      final String name = annotation.getValue( IAnnotation.ANNO_LABEL );
      if( name != null )
        return name;
    }

    return featureType.getQName().getLocalPart();
  }

}
