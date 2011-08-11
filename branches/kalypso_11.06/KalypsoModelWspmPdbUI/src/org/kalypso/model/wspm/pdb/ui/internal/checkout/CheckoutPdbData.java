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
package org.kalypso.model.wspm.pdb.ui.internal.checkout;

import java.net.URL;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.model.wspm.pdb.internal.wspm.CheckoutDataMapping;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class CheckoutPdbData extends AbstractModelObject
{
  public static final String PROPERTY_CONFIRM_EXISTING = "confirmExisting"; //$NON-NLS-1$

  private CheckoutDataMapping m_mapping;

  private boolean m_confirmExisting = false;

  private final URL m_documentBase;

  public CheckoutPdbData( final URL documentBase )
  {
    m_documentBase = documentBase;
  }

  public void init( final IDialogSettings settings )
  {
    if( settings == null )
      return;
    // TODO Auto-generated method stub
  }

  public void store( final IDialogSettings settings )
  {
    if( settings == null )
      return;
    // TODO Auto-generated method stub

  }

  public void setMapping( final CheckoutDataMapping mapping )
  {
    m_mapping = mapping;
  }

  public CheckoutDataMapping getMapping( )
  {
    return m_mapping;
  }

  public Feature[] getNewWspmElements( )
  {
    return m_mapping.getNewElements();
  }

  public boolean getConfirmExisting( )
  {
    return m_confirmExisting;
  }

  public void setConfirmExisting( final boolean confirmExisting )
  {
    final Object oldValue = m_confirmExisting;

    m_confirmExisting = confirmExisting;

    firePropertyChange( PROPERTY_CONFIRM_EXISTING, oldValue, confirmExisting );
  }

  public URL getDocumentBase( )
  {
    return m_documentBase;
  }
}