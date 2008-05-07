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
package org.kalypso.ogc.gml.featureview.dialog;

import java.util.Collection;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.annotation.AnnotationUtilities;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * A dialog which creates a feature property (i.e. it creates the inline property or allows the user to choose an
 * existing feature).
 * 
 * @author Gernot Belger
 */
public class CreateFeaturePropertyDialog implements IFeatureDialog
{
  private FeatureChange m_change = null;

  private final IFeatureChangeListener m_listener;

  private final Feature m_feature;

  private final IRelationType m_relationType;

  public CreateFeaturePropertyDialog( final IFeatureChangeListener listener, final Feature feature, final IRelationType relationType )
  {
    m_listener = listener;
    m_feature = feature;
    m_relationType = relationType;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#open(org.eclipse.swt.widgets.Shell)
   */
  public int open( final Shell shell )
  {
    final GMLWorkspace workspace = m_feature.getWorkspace();

    if( m_relationType.isInlineAble() && !m_relationType.isLinkAble() )
    {
      final IFeatureType targetFeatureType = m_relationType.getTargetFeatureType();
      final IFeatureType[] substituts = GMLSchemaUtilities.getSubstituts( targetFeatureType, workspace.getGMLSchema(), false, true );
      
      final IFeatureType newFeatureType;
      if( substituts.length == 0 )
      {
        // TODO: error message
        newFeatureType = null;
      }
      else if( substituts.length == 1  )
        newFeatureType = substituts[0];
      else
      {
      // TODO: show list of possible types to user (if more than one type is possible)
        newFeatureType = null;
      }

      if( newFeatureType == null )
        return Window.CANCEL;

      final Feature newFeature = workspace.createFeature( m_feature, m_relationType, newFeatureType );
      m_change = new FeatureChange( m_feature, m_relationType, newFeature );
      
      m_listener.openFeatureRequested( null, null ); // in order to force total refresh of feature view
      m_listener.openFeatureRequested( m_feature, m_relationType );
      
      return Window.OK;
    }

    // TODO: the other cases (choosing a feature) are not supported yet
    final String userMessage = Messages.getString("org.kalypso.ogc.gml.featureview.dialog.NotImplementedFeatureDialog.implemented"); //$NON-NLS-1$
    MessageDialog.openInformation( shell, Messages.getString("org.kalypso.ogc.gml.featureview.dialog.NotImplementedFeatureDialog.edit"), userMessage ); //$NON-NLS-1$
    
    return Window.CANCEL;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#collectChanges(java.util.Collection)
   */
  public void collectChanges( final Collection<FeatureChange> c )
  {
    if( c != null && m_change != null )
      c.add( m_change );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#getLabel()
   */
  public String getLabel( )
  {
    final String label = AnnotationUtilities.getAnnotation( m_relationType.getTargetFeatureType() ).getValue( IAnnotation.ANNO_NAME );

    final StringBuffer msg = new StringBuffer( "'" ); //$NON-NLS-1$
    msg.append( label );
    msg.append( "\' " ); //$NON-NLS-1$

    if( m_relationType.isInlineAble() )
      msg.append( Messages.getString("org.kalypso.ogc.gml.featureview.dialog.CreateFeaturePropertyDialog.2") ); //$NON-NLS-1$

    if( m_relationType.isInlineAble() && m_relationType.isLinkAble() )
      msg.append( Messages.getString("org.kalypso.ogc.gml.featureview.dialog.CreateFeaturePropertyDialog.3") ); //$NON-NLS-1$

    if( m_relationType.isLinkAble() )
      msg.append( Messages.getString("org.kalypso.ogc.gml.featureview.dialog.CreateFeaturePropertyDialog.4") ); //$NON-NLS-1$

    return msg.toString();
  }

}
