/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.ui.imports.ewawi;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.kalypso.commons.databinding.swt.FileAndHistoryData;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.model.wspm.ewawi.utils.GewShape;

/**
 * @author Holger Albert
 */
public class EwawiRiverShapeData extends AbstractModelObject
{
  public static final String PROPERTY_SHP_FILE = "shpFile"; //$NON-NLS-1$

  private static final String PROPERTY_RIVER_ID_FIELD = "riverIdField"; //$NON-NLS-1$

  private static final String PROPERTY_RIVER_NAME_FIELD = "riverNameField"; //$NON-NLS-1$

  private static final String PROPERTY_RIVER_ALIAS_FIELD = "riverAliasField"; //$NON-NLS-1$

  private final FileAndHistoryData m_shpFile;

  private String m_riverIdField;

  private String m_riverNameField;

  private String m_riverAliasField;

  public EwawiRiverShapeData( )
  {
    m_shpFile = new FileAndHistoryData( "shpFile" ); //$NON-NLS-1$
    m_riverIdField = GewShape.GKZ_FGN25;
    m_riverNameField = GewShape.GN_ACHS_08;
    m_riverAliasField = GewShape.ALIAS;
  }

  public void init( final IDialogSettings settings )
  {
    if( settings == null )
      return;

    m_shpFile.init( settings );
  }

  public void storeSettings( final IDialogSettings settings )
  {
    if( settings == null )
      return;

    m_shpFile.storeSettings( settings );
  }

  public FileAndHistoryData getShpFile( )
  {
    return m_shpFile;
  }

  public String getRiverIdField( )
  {
    return m_riverIdField;
  }

  public String getRiverNameField( )
  {
    return m_riverNameField;
  }

  public String getRiverAliasField( )
  {
    return m_riverAliasField;
  }

  public void setRiverIdField( final String riverIdField )
  {
    final String oldValue = m_riverIdField;
    m_riverIdField = riverIdField;
    firePropertyChange( PROPERTY_RIVER_ID_FIELD, oldValue, m_riverIdField );
  }

  public void setRiverNameField( final String riverNameField )
  {
    final String oldValue = m_riverNameField;
    m_riverNameField = riverNameField;
    firePropertyChange( PROPERTY_RIVER_NAME_FIELD, oldValue, m_riverNameField );
  }

  public void setRiverAliasField( final String riverAliasField )
  {
    final String oldValue = m_riverAliasField;
    m_riverAliasField = riverAliasField;
    firePropertyChange( PROPERTY_RIVER_ALIAS_FIELD, oldValue, m_riverAliasField );
  }
}