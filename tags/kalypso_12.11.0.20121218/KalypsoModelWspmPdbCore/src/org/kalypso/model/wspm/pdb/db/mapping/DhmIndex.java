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
package org.kalypso.model.wspm.pdb.db.mapping;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.UniqueConstraint;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.model.wspm.pdb.db.constants.DhmIndexConstants;

import com.vividsolutions.jts.geom.Polygon;

/**
 * @author Monika Thuel
 */
@Entity
@Table( name = "dhm_index", schema = "pdb", uniqueConstraints = @UniqueConstraint( columnNames = "filename" ) )
public class DhmIndex extends AbstractModelObject implements Serializable, DhmIndexConstants, IElementWithDates
{
  private long m_id;

  private String m_name;

  private com.vividsolutions.jts.geom.Polygon m_location;

  private String m_filename;

  private String m_mimeType;

  private Date m_creationDate;

  private Date m_editingDate;

  private String m_editingUser;

  private Date m_measurementDate;

  private String m_source;

  private String m_editor;

  private String m_measurementAccuracy;

  private String m_description;

  private String m_copyright;

  private String m_srid;

  public DhmIndex( )
  {
    m_id = 0;
    m_name = StringUtils.EMPTY;
    m_location = null;
    m_filename = StringUtils.EMPTY;
    m_mimeType = StringUtils.EMPTY;
    m_creationDate = new Date();
    m_editingDate = new Date();
    m_editingUser = StringUtils.EMPTY;
    m_measurementDate = new Date();
    m_source = StringUtils.EMPTY;
    m_editor = StringUtils.EMPTY;
    m_measurementAccuracy = StringUtils.EMPTY;
    m_description = StringUtils.EMPTY;
    m_copyright = StringUtils.EMPTY;
  }

  public DhmIndex( final long id, final String name, final com.vividsolutions.jts.geom.Polygon location, final String filename, final String mimetype, final Date creationDate, final Date editingDate, final String editingUser, final Date measurementDate, final String source, final String editor, final String measurementAccuracy, final String description, final String copyright )
  {
    m_id = id;

    /* protect against empty name here; maybe fixes bug with oracle DhmIndex upload */
    m_name = name == null ? StringUtils.EMPTY : name;

    m_location = location;
    m_filename = filename;
    m_mimeType = mimetype;
    m_creationDate = creationDate;
    m_editingDate = editingDate;
    m_editingUser = editingUser;
    m_measurementDate = measurementDate;
    m_source = source;
    m_editor = editor;
    m_measurementAccuracy = measurementAccuracy;
    m_description = description;
    m_copyright = copyright;
  }

  @Id
  @Column( name = "id", unique = true, nullable = false, precision = 20, scale = 0 )
  @GeneratedValue( strategy = GenerationType.SEQUENCE, generator = "dhmindex_id_seq" )
  @SequenceGenerator( name = "dhmindex_id_seq", sequenceName = "pdb.seq_pdb" )
  public long getId( )
  {
    return m_id;
  }

  public void setId( final long id )
  {
    final long oldValue = m_id;

    m_id = id;

    firePropertyChange( PROPERTY_ID, oldValue, m_id );
  }

  @Column( name = "name", nullable = false, length = 100 )
  public String getName( )
  {
    return m_name;
  }

  public void setName( final String name )
  {
    final String oldValue = m_name;

    m_name = name;

    firePropertyChange( PROPERTY_NAME, oldValue, m_name );
  }

  @Column( name = "location", columnDefinition = "Geometry" )
  public com.vividsolutions.jts.geom.Polygon getLocation( )
  {
    return m_location;
  }

  public void setLocation( final com.vividsolutions.jts.geom.Polygon location )
  {
    final Polygon oldValue = m_location;

    m_location = location;

    firePropertyChange( PROPERTY_LOCATION, oldValue, m_location );
  }

  @Column( name = "filename", nullable = false, length = 2048 )
  public String getFilename( )
  {
    return m_filename;
  }

  public void setFilename( final String filename )
  {
    final String oldValue = m_filename;

    m_filename = filename;

    firePropertyChange( PROPERTY_FILENAME, oldValue, m_filename );
  }

  @Column( name = "mimetype", length = 50 )
  public String getMimeType( )
  {
    return m_mimeType;
  }

  public void setMimeType( final String mimetype )
  {
    final String oldValue = m_mimeType;

    m_mimeType = mimetype;

    firePropertyChange( PROPERTY_MIMETYPE, oldValue, m_mimeType );
  }

  @Override
  @Temporal( TemporalType.TIMESTAMP )
  @Column( name = "creation_date", nullable = false, length = 22 )
  public Date getCreationDate( )
  {
    return m_creationDate;
  }

  @Override
  public void setCreationDate( final Date creationDate )
  {
    final Date oldValue = m_creationDate;
    m_creationDate = creationDate;
    firePropertyChange( PROPERTY_CREATIONDATE, oldValue, m_creationDate );
  }

  @Override
  @Temporal( TemporalType.TIMESTAMP )
  @Column( name = "editing_date", nullable = false, length = 22 )
  public Date getEditingDate( )
  {
    return m_editingDate;
  }

  @Override
  public void setEditingDate( final Date editingDate )
  {
    final Date oldValue = m_editingDate;

    m_editingDate = editingDate;

    firePropertyChange( PROPERTY_EDITINGDATE, oldValue, m_editingDate );
  }

  @Override
  @Column( name = "editing_user", nullable = false, length = 50 )
  public String getEditingUser( )
  {
    return m_editingUser;
  }

  @Override
  public void setEditingUser( final String editingUser )
  {
    final String oldValue = m_editingUser;

    m_editingUser = editingUser;

    firePropertyChange( PROPERTY_EDITINGUSER, oldValue, m_editingUser );
  }

  @Override
  @Temporal( TemporalType.TIMESTAMP )
  @Column( name = "measurement_date", length = 22 )
  public Date getMeasurementDate( )
  {
    return m_measurementDate;
  }

  @Override
  public void setMeasurementDate( final Date measurementDate )
  {
    final Date oldValue = m_measurementDate;

    m_measurementDate = measurementDate;

    firePropertyChange( PROPERTY_MEASUREMENTDATE, oldValue, m_measurementDate );
  }

  @Column( name = "source", length = 255 )
  public String getSource( )
  {
    return m_source;
  }

  public void setSource( final String source )
  {
    final String oldValue = m_source;

    m_source = source;

    firePropertyChange( PROPERTY_SOURCE, oldValue, m_source );
  }

  @Column( name = "editor", length = 255 )
  public String getEditor( )
  {
    return m_editor;
  }

  public void setEditor( final String editor )
  {
    final String oldValue = m_editor;

    m_editor = editor;

    firePropertyChange( PROPERTY_EDITOR, oldValue, m_editor );
  }

  @Column( name = "measurement_accuracy", length = 50 )
  public String getMeasurementAccuracy( )
  {
    return m_measurementAccuracy;
  }

  public void setMeasurementAccuracy( final String measurementAccuracy )
  {
    final String oldValue = m_measurementAccuracy;

    m_measurementAccuracy = measurementAccuracy;

    firePropertyChange( PROPERTY_MEASUREMENTACCURACY, oldValue, m_measurementAccuracy );
  }

  @Column( name = "description", length = 255 )
  public String getDescription( )
  {
    return m_description;
  }

  public void setDescription( final String description )
  {
    final String oldValue = m_description;

    m_description = description;

    firePropertyChange( PROPERTY_DESCRIPTION, oldValue, m_description );
  }

  @Column( name = "copyright", length = 255 )
  public String getCopyright( )
  {
    return m_copyright;
  }

  public void setCopyright( final String copyright )
  {
    final String oldValue = m_copyright;

    m_copyright = copyright;

    firePropertyChange( PROPERTY_COPYRIGHT, oldValue, m_copyright );
  }

  @Column( name = "srid", nullable = false, length = 15 )
  public String getSrid( )
  {
    return m_srid;
  }

  public void setSrid( final String srid )
  {
    final String oldValue = m_srid;

    m_srid = srid;

    firePropertyChange( PROPERTY_SRID, oldValue, m_srid );
  }
}