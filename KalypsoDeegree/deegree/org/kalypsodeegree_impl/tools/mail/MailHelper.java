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

// JDK 1.3
import java.util.Date;

import javax.mail.Message;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;

import org.deegree_impl.tools.Debug;

/**
 * A helper class to create and send mail.
 * 
 * @author <a href="mailto:tfriebe@gmx.net">Torsten Friebe </a>
 * @author <a href="mailto:fretter@lat-lon.de">Klaus Fretter </a>
 * 
 * @version $Revision$
 */
public abstract class MailHelper
{
  /**
   * This method creates an email message and sends it using the J2EE mail
   * services
   * 
   * @param eMess
   *          a email message
   * 
   * @throws SendMailException
   *           an exception if the message is undeliverable
   */
  public static synchronized void createAndSendMail( MailMessage eMess, String mailHost )
      throws SendMailException
  {
    Debug.debugMethodBegin( "MailHelper", "createAndSendMail( ... )" );

    try
    {
      java.util.Properties p = System.getProperties();
      p.put( "mail.smtp.host", mailHost );

      Session session = Session.getDefaultInstance( p, null );

      // construct the message
      Message msg = new javax.mail.internet.MimeMessage( session );
      msg.setFrom( new InternetAddress( eMess.getSender() ) );

      msg.setRecipients( Message.RecipientType.TO, InternetAddress.parse( eMess.getReceiver(),
          false ) );

      msg.setSubject( eMess.getSubject() );
      msg.setContent( eMess.getMessageBody(), eMess.getMimeType() );

      msg.setHeader( "X-Mailer", "JavaMailer" );
      msg.setSentDate( new Date() );

      // send the mail off

      /** @todo using the default transport */
      //Transport transport = session.getTransport("smtp");
      Transport.send( msg );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      Debug.debugMethodEnd();
      throw new SendMailException();
    }

    Debug.debugMethodEnd();
  }
}