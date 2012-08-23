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
import java.math.BigDecimal;
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

import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.model.wspm.pdb.db.constants.DhmIndexConstants;

/**
 * @author Monika Thuel
 */
@Entity
@Table(name = "dhm_index", schema = "pdb", uniqueConstraints = @UniqueConstraint(columnNames = "filename"))
public class DhmIndex extends AbstractModelObject implements Serializable, DhmIndexConstants, IElementWithDates
{
  private BigDecimal m_id;

  private String m_name;

  private com.vividsolutions.jts.geom.Polygon m_location;

  private String m_filename;

  private String m_mimetype;

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
  }

  public DhmIndex( final BigDecimal id, final String name, final com.vividsolutions.jts.geom.Polygon location, final String filename, final String mimetype, final Date creationDate, final Date editingDate, final String editingUser, final Date measurementDate, final String source, final String editor, final String measurementAccuracy, final String description, final String copyright )
  {
    m_id = id;
    m_name = name;
    m_location = location;
    m_filename = filename;
    m_mimetype = mimetype;
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
  @Column(name = "id", unique = true, nullable = false, precision = 20, scale = 0)
  @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "dhmindex_id_seq")
  @SequenceGenerator(name = "dhmindex_id_seq", sequenceName = "pdb.seq_pdb")
  public BigDecimal getId( )
  {
    return m_id;
  }

  public void setId( final BigDecimal id )
  {
    m_id = id;
  }

  @Column(name = "name", nullable = false, length = 100)
  public String getName( )
  {
    return m_name;
  }

  public void setName( final String name )
  {
    m_name = name;
  }

  @Column(name = "location", columnDefinition = "Geometry")
  public com.vividsolutions.jts.geom.Polygon getLocation( )
  {
    return m_location;
  }

  public void setLocation( final com.vividsolutions.jts.geom.Polygon location )
  {
    m_location = location;
  }

  @Column(name = "filename", nullable = false, length = 2048)
  public String getFilename( )
  {
    return m_filename;
  }

  public void setFilename( final String filename )
  {
    m_filename = filename;
  }

  @Column(name = "mimetype", length = 50)
  public String getMimetype( )
  {
    return m_mimetype;
  }

  public void setMimetype( final String mimetype )
  {
    m_mimetype = mimetype;
  }

  @Override
  @Temporal(TemporalType.TIMESTAMP)
  @Column(name = "creation_date", nullable = false, length = 22)
  public Date getCreationDate( )
  {
    return m_creationDate;
  }

  public void setCreationDate( final Date creationDate )
  {
    final Object oldValue = m_creationDate;

    m_creationDate = creationDate;

    firePropertyChange( PROPERTY_CREATIONDATE, oldValue, creationDate );
  }

  @Override
  @Temporal(TemporalType.TIMESTAMP)
  @Column(name = "editing_date", nullable = false, length = 22)
  public Date getEditingDate( )
  {
    return m_editingDate;
  }

  public void setEditingDate( final Date editingDate )
  {
    final Object oldValue = m_editingDate;

    m_editingDate = editingDate;

    firePropertyChange( PROPERTY_EDITINGDATE, oldValue, editingDate );
  }

  @Override
  @Column(name = "editing_user", nullable = false, length = 50)
  public String getEditingUser( )
  {
    return m_editingUser;
  }

  public void setEditingUser( final String editingUser )
  {
    final Object oldValue = m_editingUser;

    m_editingUser = editingUser;

    firePropertyChange( PROPERTY_EDITINGUSER, oldValue, editingUser );
  }

  @Override
  @Temporal(TemporalType.TIMESTAMP)
  @Column(name = "measurement_date", length = 22)
  public Date getMeasurementDate( )
  {
    return m_measurementDate;
  }

  public void setMeasurementDate( final Date measurementDate )
  {
    final Object oldValue = m_measurementDate;

    m_measurementDate = measurementDate;

    firePropertyChange( PROPERTY_MEASUREMENTDATE, oldValue, measurementDate );
  }

  @Column(name = "source", length = 255)
  public String getSource( )
  {
    return m_source;
  }

  public void setSource( final String source )
  {
    m_source = source;
  }

  @Column(name = "editor", length = 255)
  public String getEditor( )
  {
    return m_editor;
  }

  public void setEditor( final String editor )
  {
    m_editor = editor;
  }

  @Column(name = "measurement_accuracy", length = 50)
  public String getMeasurementAccuracy( )
  {
    return m_measurementAccuracy;
  }

  public void setMeasurementAccuracy( final String measurementAccuracy )
  {
    m_measurementAccuracy = measurementAccuracy;
  }

  @Column(name = "description", length = 255)
  public String getDescription( )
  {
    return m_description;
  }

  public void setDescription( final String description )
  {
    m_description = description;
  }

  @Column(name = "copyright", length = 255)
  public String getCopyright( )
  {
    return m_copyright;
  }

  public void setCopyright( final String copyright )
  {
    m_copyright = copyright;
  }

  @Column(name = "srid", nullable = false, length = 15)
  public String getSrid( )
  {
    return m_srid;
  }

  public void setSrid( final String srid )
  {
    m_srid = srid;
  }
}
