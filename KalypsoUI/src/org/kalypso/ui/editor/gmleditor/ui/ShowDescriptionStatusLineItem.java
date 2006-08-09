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
package org.kalypso.ui.editor.gmleditor.ui;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.internal.util.StatusLineContributionItem;
import org.kalypso.gmlschema.adapter.IAnnotation;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * A status line item, which shows the description of the currently selected feature.
 * 
 * @author Gernot Belger
 */
public class ShowDescriptionStatusLineItem extends StatusLineContributionItem
{
  private final ISelectionChangedListener m_listener = new ISelectionChangedListener()
  {
    public void selectionChanged( final SelectionChangedEvent event )
    {
      final ISelection selection = event.getSelection();
      handleSelectionChanged( selection );
    }
  };

  private IEditorPart m_targetEditor;

  public ShowDescriptionStatusLineItem( final String id )
  {
    super( id );
  }

  public ShowDescriptionStatusLineItem( final String id, final int charWidth )
  {
    super( id, charWidth );
  }

  /**
   * @see org.eclipse.jface.action.ContributionItem#dispose()
   */
  @Override
  public void dispose( )
  {
    unhookEditor();

    super.dispose();
  }

  public void setActiveEditor( final IEditorPart targetEditor )
  {
    unhookEditor();

    m_targetEditor = targetEditor;

    hookEditor();
  }

  private void hookEditor( )
  {
    if( m_targetEditor != null )
    {
      final ISelectionProvider selectionProvider = m_targetEditor.getSite().getSelectionProvider();
      if( selectionProvider != null )
        selectionProvider.addSelectionChangedListener( m_listener );
    }
  }

  private void unhookEditor( )
  {
    if( m_targetEditor != null )
    {
      final ISelectionProvider selectionProvider = m_targetEditor.getSite().getSelectionProvider();
      if( selectionProvider != null )
        selectionProvider.removeSelectionChangedListener( m_listener );
    }
  }

  protected void handleSelectionChanged( final ISelection selection )
  {
    setText( "" );

    if( selection instanceof IFeatureSelection )
    {
      final Feature feature = FeatureSelectionHelper.getFirstFeature( (IFeatureSelection) selection );
      if( feature != null )
      {
        final String desc = FeatureHelper.getAnnotationValue( feature, IAnnotation.ANNO_DESCRIPTION );
        setText( desc );
      }
    }

  }

}
