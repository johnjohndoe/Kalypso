/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de

 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.tools.mail;

/**
 * This class encapsulates all the info need to send an email message. This
 * object is passed to the MailerEJB sendMail(...) method.
 * 
 * @author <a href="mailto:tfriebe@gmx.net">Torsten Friebe </a>
 * 
 * @version $Revision$
 */
public class EMailMessage implements MailMessage, java.io.Serializable
{
  private String emailReceiver;

  private String htmlContents;

  private String mimeType;

  private String sender;

  private String subject;

  /**
   * Creates a new EMailMessage object.
   */
  private EMailMessage()
  {
    try
    {
      this.setMimeType( MailMessage.PLAIN_TEXT );
    }
    catch( Exception ex )
    {
      // nothing to do
    }
  }

  /**
   * Creates a new mail message with MIME type text/plain.
   * 
   * @param from
   *          the sender
   * 
   * @param to
   *          the receiver list
   * 
   * @param subject
   *          the subject
   * 
   * @param messageBody
   *          the content of the message
   */
  public EMailMessage( String from, String to, String subject, String messageBody )
  {
    this();

    this.setSender( from );
    this.setReceiver( to );
    this.setSubject( subject );
    this.setMessageBody( messageBody );
  }

  /**
   * Creates a new mail message with the given MIME type.
   * 
   * @param from
   *          the sender
   * 
   * @param to
   *          the receiver list
   * 
   * @param subject
   *          the subject
   * 
   * @param messageBody
   *          the content of the message
   * 
   * @param mimeType
   *          the MIME type of the message body
   * 
   * @throws UnknownMimeTypeException
   *           if the given mimeType is not supported
   */
  public EMailMessage( String from, String to, String subject, String messageBody, String mimeType )
      throws UnknownMimeTypeException
  {
    this( from, to, subject, messageBody );
    this.setMimeType( mimeType );
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    return subject + " " + emailReceiver + " " + htmlContents;
  }

  /**
   * Method declaration
   * 
   * 
   * @return
   */
  public String getSender()
  {
    // Write your code here
    return this.sender;
  }

  /**
   * Method declaration
   * 
   *  
   */
  public String getMessageBody()
  {
    // Write your code here
    return this.htmlContents;
  }

  /**
   * Method declaration
   * 
   * 
   * @return
   */
  public String getReceiver()
  {
    // Write your code here
    return this.emailReceiver;
  }

  /**
   * Method declaration
   * 
   * 
   * @param to
   *  
   */
  public void setReceiver( String to )
  {
    this.emailReceiver = to;
  }

  /**
   * Method declaration
   * 
   * 
   * @param message
   *  
   */
  public void setMessageBody( String message )
  {
    this.htmlContents = message;
  }

  /**
   * Method declaration
   * 
   * 
   * @param from
   *  
   */
  public void setSender( String from )
  {
    this.sender = from;
  }

  /**
   * Method declaration
   * 
   * 
   * @param title
   *  
   */
  public void setSubject( String title )
  {
    this.subject = title;
  }

  /**
   * 
   * 
   * @return
   */
  public String getSubject()
  {
    return subject;
  }

  /**
   * 
   * 
   * @param mimeType
   * 
   * @throws UnknownMimeTypeException
   */
  public void setMimeType( String mimeType ) throws UnknownMimeTypeException
  {
    if( mimeType.equalsIgnoreCase( MailMessage.PLAIN_TEXT ) )
    {
      this.mimeType = mimeType;
    }
    else if( mimeType.equalsIgnoreCase( MailMessage.TEXT_HTML ) )
    {
      this.mimeType = mimeType;
    }
    else
    {
      throw new UnknownMimeTypeException( getClass().getName(), mimeType );
    }
  }

  /**
   * 
   * 
   * @return
   */
  public String getMimeType()
  {
    return this.mimeType;
  }
}