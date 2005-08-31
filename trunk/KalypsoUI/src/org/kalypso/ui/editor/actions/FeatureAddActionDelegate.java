/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ui.editor.actions;

import java.util.List;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IActionDelegate;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.CommandableFeatureSelection;
import org.kalypso.ogc.gml.selection.IFeatureThemeSelection;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;

/**
 * FeatureRemoveActionDelegate
 * <p>
 * 
 * created by
 * 
 * @author doemming (24.05.2005)
 */
public class FeatureAddActionDelegate implements IActionDelegate
{

  private IStructuredSelection m_selection = null;

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    if( action.isEnabled() && m_selection != null )
    {
      if( m_selection instanceof IFeatureThemeSelection )
      {
        final IKalypsoFeatureTheme theme = ( (IFeatureThemeSelection)m_selection ).getKalypsoFeatureTheme();
        final CommandableWorkspace workspace = theme.getWorkspace();
        final FeatureList featureList = theme.getFeatureList();

        final Feature parentFeature = featureList.getParentFeature();
        // TODO change featurelist and remove cast
        // TODO ask for FeatureType (substitutiongroup)
        final FeatureAssociationTypeProperty ftp = (FeatureAssociationTypeProperty)featureList
            .getParentFeatureTypeProperty();
        int pos = 0; // TODO get pos from somewhere
        final AddFeatureCommand command = new AddFeatureCommand( workspace, ftp.getAssociationFeatureType(),
            parentFeature, ftp.getName(), pos );
        try
        {
          workspace.postCommand( command );
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }
      }
      else if( m_selection instanceof CommandableFeatureSelection )
      {
        CommandableFeatureSelection selection = (CommandableFeatureSelection)m_selection;
        CommandableWorkspace cWorkspace = selection.getCommandableWorkspace();
        //is always a Feature since object contribution points to feature
        Feature selectedFeature = (Feature)m_selection.getFirstElement();
        FeatureType featureType = selectedFeature.getFeatureType();
        Feature parentFeature = cWorkspace.getParentFeature( selectedFeature );
        FeatureType parentFtp = parentFeature.getFeatureType();
        FeatureTypeProperty[] properties = parentFtp.getProperties();
        String propName = null;
        FeatureType ftp = null;
        for( int i = 0; i < properties.length; i++ )
        {
          FeatureTypeProperty property = properties[i];
          if( property.getName().equals( featureType.getName() ) )
          {
            ftp = featureType;
          }
          if( property instanceof FeatureAssociationTypeProperty )
          {
            propName = property.getName();
            FeatureType[] ftps = ( (FeatureAssociationTypeProperty)property ).getAssociationFeatureTypes();
            for( int j = 0; j < ftps.length; j++ )
            {
              FeatureType ft = ftps[j];
              String name = ftps[j].getName();
              if( name.equals( featureType.getName() ) )
              {
                ftp = ft;
                break;
              }
            }
          }
          if( ftp != null )
            break;
        }
        //guess insert position in list
        int pos = 0;
        if( parentFtp.isListProperty( propName ) )
        {
          List list = (List)parentFeature.getProperty( propName );
          pos = list.indexOf( selectedFeature );
        }
        try
        {
          AddFeatureCommand command = new AddFeatureCommand( cWorkspace, ftp, parentFeature, propName, pos );
          cWorkspace.postCommand( command );
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }
      }
    }

  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
    action.setEnabled( false );
    if( selection instanceof IStructuredSelection )
    {
      m_selection = (IStructuredSelection)selection;
      if( selection instanceof IFeatureThemeSelection )
      {
        // TODO check maxOccurs
        action.setEnabled( true );
      }
      else if( m_selection instanceof CommandableFeatureSelection )
      {
        if( m_selection.getFirstElement() instanceof Feature )
          action.setEnabled( true );
      }
    }
  }
}
