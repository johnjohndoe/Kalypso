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
package org.kalypso.ui.editor.actions;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;

import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.PropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;

/**
 * This command edits a feature's value
 * 
 * @author Stefan Kurzbach
 */
public class EditFeatureValueCommand implements ICommand
{

  private final GMLWorkspace m_workspace;

  private final Feature m_feature;

  private final IPropertyType m_propertyType;

  /**
   * Creates a new EditFeatureValueCommand that knows it's GMLWorkspace and the feature and property it should edit
   */
  public EditFeatureValueCommand( final GMLWorkspace workspace, final Feature feature, final IPropertyType propertyType )
  {
    m_workspace = workspace;
    m_feature = feature;
    m_propertyType = propertyType;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return "Editiert " + m_feature + ": " + m_propertyType;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable( )
  {
    return true;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process( ) throws Exception
  {
    System.out.println(m_propertyType.getClass().getName());
    if( m_propertyType instanceof PropertyType )
    {
      final IPropertyType propertyType = m_propertyType;
      final FeatureVisitor visitor = new FeatureVisitor()
      {

        /**
         * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
         */
        public boolean visit( final Feature f )
        {
          Object property = f.getProperty( propertyType );
          try
          {
            final Object newValue = 1.1f;
            f.setProperty( propertyType, newValue );
            System.out.println(f);
          }
          catch( Exception e )
          {
            e.printStackTrace();
          }
          return true;
        }

      };
      m_workspace.accept( visitor, m_feature, FeatureVisitor.DEPTH_ZERO, new IPropertyType[] { m_propertyType } );

      final ArrayList<Feature> featureList = new ArrayList<Feature>();
      featureList.add( m_feature );
      m_workspace.fireModellEvent( new FeaturesChangedModellEvent( m_workspace, featureList ) );
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
    process();
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {

  }

}
