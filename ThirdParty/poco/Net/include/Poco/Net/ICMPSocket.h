//
// ICMPSocket.h
//
// $Id: //poco/1.4/Net/include/Poco/Net/ICMPSocket.h#1 $
//
// Library: Net
// Package: ICMP
// Module:  ICMPSocket
//
// Definition of the ICMPSocket class.
//
// Copyright (c) 2006, Applied Informatics Software Engineering GmbH.
// and Contributors.
//
// Permission is hereby granted, free of charge, to any person or organization
// obtaining a copy of the software and accompanying documentation covered by
// this license (the "Software") to use, reproduce, display, distribute,
// execute, and transmit the Software, and to prepare derivative works of the
// Software, and to permit third-parties to whom the Software is furnished to
// do so, all subject to the following:
// 
// The copyright notices in the Software and this entire statement, including
// the above license grant, this restriction and the following disclaimer,
// must be included in all copies of the Software, in whole or in part, and
// all derivative works of the Software, unless such copies or derivative
// works are solely in the form of machine-executable object code generated by
// a source language processor.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE, TITLE AND NON-INFRINGEMENT. IN NO EVENT
// SHALL THE COPYRIGHT HOLDERS OR ANYONE DISTRIBUTING THE SOFTWARE BE LIABLE
// FOR ANY DAMAGES OR OTHER LIABILITY, WHETHER IN CONTRACT, TORT OR OTHERWISE,
// ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
//


#ifndef Net_ICMPSocket_INCLUDED
#define Net_ICMPSocket_INCLUDED


#include "Poco/Net/Net.h"
#include "Poco/Net/Socket.h"


namespace Poco {
namespace Net {


class Net_API ICMPSocket: public Socket
	/// This class provides an interface to an
	/// ICMP client socket.
{
public:
	ICMPSocket(IPAddress::Family family, int dataSize = 48, int ttl = 128, int timeout = 500000);
		/// Creates an unconnected ICMP socket.
		///
		/// The socket will be created for the
		/// given address family.

	ICMPSocket(const Socket& socket);
		/// Creates the ICMPSocket with the SocketImpl
		/// from another socket. The SocketImpl must be
		/// a DatagramSocketImpl, otherwise an InvalidArgumentException
		/// will be thrown.

	~ICMPSocket();
		/// Destroys the ICMPSocket.

	ICMPSocket& operator = (const Socket& socket);
		/// Assignment operator.
		///
		/// Releases the socket's SocketImpl and
		/// attaches the SocketImpl from the other socket and
		/// increments the reference count of the SocketImpl.	

	int sendTo(const SocketAddress& address, int flags = 0);
		/// Sends an ICMP request through
		/// the socket to the given address.
		///
		/// Returns the number of bytes sent.

	int receiveFrom(SocketAddress& address, int flags = 0);
		/// Receives data from the socket.
		/// Stores the address of the sender in address.
		///
		/// Returns the time elapsed since the originating 
		/// request was sent.

	int dataSize() const;
		/// Returns the data size in bytes.

	int ttl() const;
		/// Returns the Time-To-Live value.

	int timeout() const;
		/// Returns the socket timeout value.

protected:
	ICMPSocket(SocketImpl* pImpl);
		/// Creates the Socket and attaches the given SocketImpl.
		/// The socket takes owership of the SocketImpl.
		///
		/// The SocketImpl must be a ICMPSocketImpl, otherwise
		/// an InvalidArgumentException will be thrown.

private:
	int _dataSize; 
	int _ttl;
	int _timeout;
};


//
// inlines
//
inline int ICMPSocket::dataSize() const
{
	return _dataSize;
}


inline int ICMPSocket::ttl() const
{
	return _ttl;
}


inline int ICMPSocket::timeout() const
{
	return _timeout;
}


} } // namespace Poco::Net


#endif // Net_ICMPSocket_INCLUDED
