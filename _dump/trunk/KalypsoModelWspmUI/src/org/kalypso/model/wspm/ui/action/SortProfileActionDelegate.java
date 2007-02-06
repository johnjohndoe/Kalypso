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
package org.kalypso.model.wspm.ui.action;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.actions.ActionDelegate;
import org.eclipse.ui.dialogs.ListDialog;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.adapter.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.ogc.gml.AnnotationUtilities;
import org.kalypso.ui.editor.gmleditor.ui.FeatureAssociationTypeElement;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * @author Gernot Belger
 */
public class SortProfileActionDelegate extends ActionDelegate implements IObjectActionDelegate
{
  private FeatureAssociationTypeElement m_profileFate;

  private static final String STR_TITLE = "Features sortieren";

  /**
   * @see org.eclipse.ui.IObjectActionDelegate#setActivePart(org.eclipse.jface.action.IAction,
   *      org.eclipse.ui.IWorkbenchPart)
   */
  public void setActivePart( final IAction action, final IWorkbenchPart targetPart )
  {
  }

  /**
   * @see org.eclipse.ui.actions.ActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  @Override
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    m_profileFate = null;

    if( selection instanceof IStructuredSelection )
    {
      final Object firstElement = ((IStructuredSelection) selection).getFirstElement();
      if( firstElement instanceof FeatureAssociationTypeElement )
      {
        final FeatureAssociationTypeElement fate = (FeatureAssociationTypeElement) firstElement;
        final IRelationType rt = fate.getAssociationTypeProperty();
        final IFeatureType targetFeatureType = rt.getTargetFeatureType();
        if( WspmProfile.QNAME_PROFILE.equals( targetFeatureType.getQName() ) )
          m_profileFate = fate;
      }
    }

    if( action != null )
      action.setEnabled( m_profileFate != null );
  }

  /**
   * @see org.eclipse.ui.actions.ActionDelegate#runWithEvent(org.eclipse.jface.action.IAction,
   *      org.eclipse.swt.widgets.Event)
   */
  @Override
  public void runWithEvent( final IAction action, final Event event )
  {
    if( m_profileFate == null )
      return;

    final Shell shell = event.display.getActiveShell();

    final Feature parentFeature = m_profileFate.getParentFeature();
    final IRelationType rt = m_profileFate.getAssociationTypeProperty();

    final IFeatureType targetFeatureType = rt.getTargetFeatureType();
    final IPropertyType ptToSort = askForPropertyToSort( shell, targetFeatureType );
    if( ptToSort == null )
      return;

    final FeatureList profiles = (FeatureList) parentFeature.getProperty( rt );
    try
    {
      sort( profiles, ptToSort );
    }
    catch( final Throwable e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      ErrorDialog.openError( shell, STR_TITLE, "Liste konnte nicht sortiert werden", status );
    }
  }

  private IPropertyType askForPropertyToSort( final Shell shell, final IFeatureType ft )
  {
    final IPropertyType[] properties = ft.getProperties();
    final List<IPropertyType> props = new ArrayList<IPropertyType>( properties.length );
    for( final IPropertyType type : properties )
    {
      if( type instanceof IValuePropertyType )
        props.add( type );
    }

    if( props.size() == 0 )
    {
      MessageDialog.openInformation( shell, STR_TITLE, "Keine sortierbare Eigenschaft vorhanden. Abbruch." );
      return null;
    }

    final ListDialog dialog = new ListDialog( shell );
    dialog.setTitle( STR_TITLE );
    dialog.setMessage( "W‰hlen Sie die Eigenschaft, nach der sortiert werden soll." );
    dialog.setAddCancelButton( true );
    // dialog.setDialogBoundsSettings( settings, strategy );

    dialog.setContentProvider( new ArrayContentProvider() );
    dialog.setInput( props );
    dialog.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( final Object element )
      {
        final IAnnotation annotation = AnnotationUtilities.getAnnotation( element );
        if( annotation != null )
          return annotation.getLabel();

        return super.getText( element );
      }
    } );
    dialog.setInitialSelections( new Object[] { props.get( 0 ) } );

    if( dialog.open() != Window.OK )
      return null;

    final Object[] result = dialog.getResult();
    if( result == null || result.length == 0 )
      return null;

    return (IPropertyType) result[0];
  }

  private void sort( final FeatureList list, final IPropertyType propertyToSort )
  {
    if( propertyToSort.isList() )
      throw new IllegalArgumentException( "Cannot sort by a list-property" );

    if( !(propertyToSort instanceof IValuePropertyType) )
      throw new IllegalArgumentException( "Can only sort value-properties!" );

    final IValuePropertyType vpt = (IValuePropertyType) propertyToSort;
    final Class valueClass = vpt.getValueClass();

    if( !(Comparable.class.isAssignableFrom( valueClass )) )
      throw new IllegalArgumentException( "Can only sort comparable data objects." );

    // TODO: undoable!

    final Comparator featureComparator = new FeatureComparator( propertyToSort );
    Collections.sort( list, featureComparator );

    final Feature parentFeature = list.getParentFeature();
    final GMLWorkspace workspace = parentFeature.getWorkspace();
    workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, parentFeature, (Feature[]) null, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_MOVE ) );
  }
}
