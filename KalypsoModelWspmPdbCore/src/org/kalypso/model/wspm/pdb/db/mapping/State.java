package org.kalypso.model.wspm.pdb.db.mapping;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.UniqueConstraint;

import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.model.wspm.pdb.db.constants.StateConstants;

/**
 * State generated by hbm2java
 */
@Entity
@Table(name = "state", schema = "pdb_admin", uniqueConstraints = @UniqueConstraint(columnNames = "name"))
public class State extends AbstractModelObject implements Serializable, StateConstants, IElementWithDates
{
  private BigDecimal id;

  private String name;

  private char isstatezero;

  private Date creationDate;

  private Date editingDate;

  private String editingUser;

  private Date measurementDate;

  private String source;

  private String description;

  private Set<CrossSection> crossSections = new HashSet<CrossSection>( 0 );

  private Set<Document> documents = new HashSet<Document>( 0 );

  public State( )
  {
  }

  public State( final BigDecimal id, final String name, final char isstatezero, final Date creationDate, final Date editingDate, final String editingUser )
  {
    this.id = id;
    this.name = name;
    this.isstatezero = isstatezero;
    this.creationDate = creationDate;
    this.editingDate = editingDate;
    this.editingUser = editingUser;
  }

  public State( final BigDecimal id, final String name, final char isstatezero, final Date creationDate, final Date editingDate, final String editingUser, final Date measurementDate, final String source, final String description, final Set<CrossSection> crossSections, final Set<Document> documents  )
  {
    this.id = id;
    this.name = name;
    this.isstatezero = isstatezero;
    this.creationDate = creationDate;
    this.editingDate = editingDate;
    this.editingUser = editingUser;
    this.measurementDate = measurementDate;
    this.source = source;
    this.description = description;
    this.crossSections = crossSections;
    this.documents = documents;
  }

  @Id
  @Column(name = "id", unique = true, nullable = false, precision = 20, scale = 0)
  @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "state_id_seq")
  @SequenceGenerator(name = "state_id_seq", sequenceName = "seq_pdb")
  public BigDecimal getId( )
  {
    return this.id;
  }

  public void setId( final BigDecimal id )
  {
    final Object oldValue = this.id;

    this.id = id;

    firePropertyChange( PROPERTY_ID, oldValue, id );
  }

  @Column(name = "name", unique = true, nullable = false, length = 100)
  public String getName( )
  {
    return this.name;
  }

  public void setName( final String name )
  {
    final Object oldValue = this.name;

    this.name = name;

    firePropertyChange( PROPERTY_NAME, oldValue, name );
  }

  @Column(name = "isstatezero", nullable = false, length = 1)
  public char getIsstatezero( )
  {
    return this.isstatezero;
  }

  public void setIsstatezero( final char isstatezero )
  {
    final Object oldValue = this.isstatezero;

    this.isstatezero = isstatezero;

    firePropertyChange( PROPERTY_ISSTATEZERO, oldValue, isstatezero );
  }

  @Override
  @Temporal(TemporalType.TIMESTAMP)
  @Column(name = "creation_date", nullable = false, length = 22)
  public Date getCreationDate( )
  {
    return this.creationDate;
  }

  public void setCreationDate( final Date creationDate )
  {
    final Object oldValue = this.creationDate;

    this.creationDate = creationDate;

    firePropertyChange( PROPERTY_CREATIONDATE, oldValue, creationDate );
  }

  @Override
  @Temporal(TemporalType.TIMESTAMP)
  @Column(name = "editing_date", nullable = false, length = 22)
  public Date getEditingDate( )
  {
    return this.editingDate;
  }

  public void setEditingDate( final Date editingDate )
  {
    final Object oldValue = this.editingDate;

    this.editingDate = editingDate;

    firePropertyChange( PROPERTY_EDITINGDATE, oldValue, editingDate );
  }

  @Override
  @Column(name = "editing_user", nullable = false, length = 50)
  public String getEditingUser( )
  {
    return this.editingUser;
  }

  public void setEditingUser( final String editingUser )
  {
    final Object oldValue = this.editingUser;

    this.editingUser = editingUser;

    firePropertyChange( PROPERTY_EDITINGUSER, oldValue, editingUser );
  }

  @Override
  @Temporal(TemporalType.TIMESTAMP)
  @Column(name = "measurement_date", length = 22)
  public Date getMeasurementDate( )
  {
    return this.measurementDate;
  }

  public void setMeasurementDate( final Date measurementDate )
  {
    final Object oldValue = this.measurementDate;

    this.measurementDate = measurementDate;

    firePropertyChange( PROPERTY_MEASUREMENTDATE, oldValue, measurementDate );
  }

  @Column(name = "source")
  public String getSource( )
  {
    return this.source;
  }

  public void setSource( final String source )
  {
    final Object oldValue = this.source;

    this.source = source;

    firePropertyChange( PROPERTY_SOURCE, oldValue, source );
  }

  @Column(name = "description")
  public String getDescription( )
  {
    return this.description;
  }

  public void setDescription( final String description )
  {
    final Object oldValue = this.description;

    this.description = description;

    firePropertyChange( PROPERTY_DESCRIPTION, oldValue, description );
  }

  @OneToMany(fetch = FetchType.LAZY, mappedBy = "state")
  public Set<CrossSection> getCrossSections( )
  {
    return this.crossSections;
  }

  public void setCrossSections( final Set<CrossSection> crossSections )
  {
    this.crossSections = crossSections;
  }

  @OneToMany(fetch = FetchType.LAZY, mappedBy = "state")
  public Set<Document> getDocuments( )
  {
    return this.documents;
  }

  public void setDocuments( final Set<Document> documents )
  {
    this.documents = documents;
  }

}
